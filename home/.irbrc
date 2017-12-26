# -*- Mode: ruby -*-

require 'irb/completion'

require 'english'
require 'yaml'
require 'stringio'
require 'zlib'
require 'time'
require 'fileutils'
require 'pp'
require 'ostruct'
require 'uri'
require 'cgi'

require 'rexml/document'
require 'rexml/xpath'

begin
  require 'rubygems'
  require 'chronic'
rescue LoadError
end

class TimeTracker
  def initialize
    @tracks = Hash.new(0)
  end

  attr_reader :tracks

  def add(name)
    start = Time.now.to_f
    yield
  ensure
    elapsed = Time.now.to_f - start
    @tracks[name] += elapsed
  end

  def to_s
    @tracks.sort_by{|_,v| v}.each do |name, elapsed|
      printf("%10s: %f\n", name, elapsed)
    end
  end
end

unless Object.const_defined?(:HISTFILE)
  HISTFILE    = File.join(ENV['HOME'], '.irbhst')
  MAXHISTSIZE = 100
  if defined? Readline::HISTORY
    if File.exist?(HISTFILE)
      lines = IO.readlines(HISTFILE).map(&:chomp)
      Readline::HISTORY.push(*lines)
    end
    at_exit do
      lines = Readline::HISTORY.to_a.reverse.uniq.reverse
      lines = lines[-MAXHISTSIZE, MAXHISTSIZE] if lines.count > MAXHISTSIZE
      File.open(HISTFILE, 'w') { |f| f.puts(lines.join($RS)) }
    end
  end
end

if ENV.include?('RAILS_ENV') && !defined?(RAILS_DEFAULT_LOGGER)
  require 'logger'
  RAILS_DEFAULT_LOGGER = Logger.new(STDOUT)
end

if defined? Rails
  begin
    require 'factory_girl'
    # fn = Rails.root.join 'spec','support','factories'
    # FactoryGirl.definition_file_paths = [fn.to_s] if fn.exist?
    # FactoryGirl.find_definitions
  rescue LoadError
  end

  def rpp
    jj app.response.body
  end

  def login(email = 'sandy@mailinator.com')
    @s ||= User.find_by_email!(email)
    auth = ActionController::HttpAuthentication::Basic.encode_credentials(@s.email, @s.auth_uid)
    app.post '/service/users/auth/developer/callback', nil, 'HTTP_AUTHORIZATION' => auth
    @s
  end

  module Rails
    def self.models
      @models ||= Rails.root.join('app', 'models').children.map do |pn|
        pn.extname == '.rb' or
          next
        name = pn.basename('.rb').to_s.classify
        begin
          model = Kernel.const_get(name)
          if model.respond_to?(:scope)
            model.module_exec do
              scope :like, ->(matcher) {
                m = matcher.first
                where arel_table[m[0]].matches("%#{m[1]}%")
              }
            end
          end
          model
        rescue => ex
          $stderr.puts "*** #{ex}", ex.backtrace[0..5]
        end
      end.compact
    end
    models
  end

  def list_models
    puts
    Rails.models.each do |model|
      model.new
      puts model.inspect
    end
    puts
  end

  def set_password(email, new_password)
    user = User.find_by_email!(email)
    def user.password_complexity() end # disable requirements
    user.update_attributes!(password: new_password)
  end
end

class Object
  def findm(match)
    methods.map { |name| name =~ /#{match}/ ? name : nil }.compact
  end
end

def reload
  load(__FILE__)
end

def jj(json)
  pp JSON.parse(json)
  nil
end

def dump(obj, with_methods = false)
  m = caller.first.match(%r{([^/:]+:\d+:)in `([^']+)/})
  title = m ? m[1] + m[2] : nil
  p [title, obj.class, obj]
  p obj.public_methods.sort if with_methods
  nil
end

def dump_methods(obj, base_class = Object)
  dump_in_cols((obj.public_methods - base_class.public_methods).sort)
end

def dump_in_cols(list, colbuf = 4, width = ENV['COLUMNS'].to_i)
  return unless list && list.any?

  colbuf = 4
  width  = 80 if width < 1
  max    = list.max_by(&:length)
  cols   = width / (max + colbuf)

  if cols <= 1
    list.each { |i| puts i }
    return
  end

  # build a set of format strings depending on how many columns to display on each line
  colbuf = ' ' * colbuf
  fmts = cols.times.each_with_object({}) do |i, h|
    h[i] = Array.new(i + 1) { |l| l << "%-#{max}s" }.join(colbuf)
  end

  lines = (list.count.to_f / cols).ceil
  lines.times do |line|
    vals = []
    cols.times do |col|
      offset = line + (lines * col)
      # p [:vals, vals]
      # p ({offset: offset, line: line, offset: offset, length: list.length})
      break unless offset < list.length
      vals << list[offset]
    end
    puts fmts[vals.count] % vals
  end

  nil
end

# returns new hash of changes made between h1 and h2 keys
def hash_diff(h1, h2)
  h2.inject({}) do |results, (k2, v2)|
    if h1.key?(k2)
      v1 = h1[k2]
      if v1.is_a? Numeric
        result = v2 - v1
      elsif v1 != v2
        # todo: support complicated object diff (string, array, hash, etc)
        result = {orig_value: v1, new_value: v2}
      else
        result = :hash_diff_equivalent
      end
    else
      result = {new_value: v2}
    end
    result == :hash_diff_equivalent or results[k2] = result
    results
  end
end

def uri_decode(str)
  uri = URI.parse(str)
  uri.query and
    params = OpenStruct.new(CGI.parse(uri.query))
  [uri, params]
end

# crazy that CGI doesn't include the opposite of parse!
# TODO: maybe URI.encode_www_form is what i want?
def uri_encode(uri, params = nil)
  params and
    uri.query = params.to_h
                  .map { |k, v| "#{k}=#{Array(v).map { |sv| CGI.escape(sv.to_s) }.join(',')}" }
                  .join('&')
  uri.to_s
end

def capture
  yield
rescue Exception => ex
  ex
end

def prythis
  binding.pry
  yield
end

def mypp(obj, prefix: '', newline_prefix: '', indent: '  ')
  case obj
  when Hash
    puts prefix + '{'
    obj.each do |k, v|
      print(indent + (k.is_a?(Symbol) ? "#{k}:" : %("#{k}" =>)))
      mypp(v, prefix: ' ', indent: indent + indent)
    end
    puts prefix + '}'
  when Array
    puts prefix + '['
    obj.each { |v| mypp(v, indent: prefix + ' ' + indent) }
    puts prefix + ']'
  else
    puts %(#{prefix}"#{obj}",)
  end
end

puts "(loaded #{__FILE__})"
