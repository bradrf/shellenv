# -*- Mode: ruby -*-

require 'irb/completion'

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
    require 'wirble'
    Wirble.init
    Wirble.colorize
rescue LoadError
#    $stderr.puts "no wirble"
end

unless Object.const_defined?(:HISTFILE)
    HISTFILE    = File.join(ENV['HOME'],'.irbhst')
    MAXHISTSIZE = 100
    if defined? Readline::HISTORY
        if File.exist?(HISTFILE)
            lines = IO.readlines(HISTFILE).collect{|line| line.chomp}
            Readline::HISTORY.push(*lines)
        end
        Kernel::at_exit do
            lines = Readline::HISTORY.to_a.reverse.uniq.reverse
            lines = lines[-MAXHISTSIZE, MAXHISTSIZE] if lines.count > MAXHISTSIZE
            File.open(HISTFILE, 'w') {|f| f.puts(lines.join("\n"))}
        end
    end
end

if ENV.include?("RAILS_ENV") && !defined?(RAILS_DEFAULT_LOGGER)
    require "logger"
    RAILS_DEFAULT_LOGGER = Logger.new(STDOUT)
end

if defined?(Rails)
    begin
        require 'factory_girl'
        # fn = Rails.root.join 'spec','support','factories'
        # FactoryGirl.definition_file_paths = [fn.to_s] if fn.exist?
        # FactoryGirl.find_definitions
    rescue LoadError
    end

    begin
        require 'hirb'
        Hirb.enable
    rescue LoadError
    end

    def jj(json)
      pp JSON.parse(json);nil
    end

    def login(email='sandy@mailinator.com')
      @s ||= User.find_by_email!(email)
      auth = ActionController::HttpAuthentication::Basic.encode_credentials(@s.email, @s.auth_uid)
      app.post '/service/users/auth/developer/callback', nil, {'HTTP_AUTHORIZATION' => auth}
      @s
    end

    def rpp
      pp JSON.parse(app.response.body); nil
    end

    module ::Rails
      def self.models
        unless Rails.class_variable_defined? :@@models
          @@models = []
          Rails.root.join('app','models').children.map do |pn|
            next unless pn.to_s.ends_with?('.rb')
            name  = pn.basename('.rb').to_s.classify
            begin
              @@models << Kernel.const_get(name)
              # fixme:
              # *** undefined method `scope' for DirectoryEntryInfo:Class
              #
              # model.module_exec do
              #   scope :like, ->(matcher) {
              #     m = matcher.first
              #     where arel_table[m[0]].matches("%#{m[1]}%")
              #   }
              # end
            rescue Exception => ex
              $stderr.puts "*** #{ex}"
            end
          end
        end
        return @@models
      end
      self.models
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

class D
    @@d = self.new()

    def self.method_missing( sym, *args, &block )
        return @@d.method( sym ).call( *args, &block )
    end

    include FileUtils

    def ls( dir='.' )
        Dir.entries( dir )
    end

    def ls_al( dir='.' )
        Dir.foreach( dir ) do |name|
            st = File.stat( name )
            printf( "%-30s %-10s #{st.ctime}\n", name, st.size )
        end
    end
end

class Object
    def findm( match )
        methods.collect do |name|
            name if name =~ /#{match}/
        end.compact
    end
end

def reload
    load( __FILE__ )
end

def list
    [:D, :test_helper, :load_ldn, :findm, :reload, :serial2mac]
end

def dump(obj, with_methods=false)
  m = caller.first.match(/([^\/:]+:\d+:)in `([^']+)/)
  title = m ? m[1] + m[2] : nil
  p [title, obj.class, obj]
  p obj.public_methods.sort if with_methods
  nil
end

def dump_methods(obj, base_class=Object)
  dump_in_cols((obj.public_methods - base_class.public_methods).sort)
end

def dump_in_cols(list, colbuf=4, width=ENV['COLUMNS'].to_i)
  return unless list && list.any?

  colbuf = 4
  width  = 80 if width < 1
  max    = list.max{|a,b| a.length <=> b.length}.length
  cols   = width / (max + colbuf)

  if cols <= 1
    list.each{|i| puts i}
    return
  end

  # build a set of format strings depending on how many columns to display on each line
  colbuf = ' '*colbuf
  fmts = cols.times.inject({}) do |h, i|
    i += 1
    h[i] = i.times.inject([]) {|l| l << "%-#{max}s"}.join(colbuf)
    h
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
def uri_encode(uri, params = nil)
  params and
    uri.query = params.to_h
                      .map { |k, v| "#{k}=#{Array(v).map { |sv| CGI.escape(sv.to_s) }.join(',')}" }
                      .join('&')
  uri.to_s
end

puts "(loaded #{__FILE__})"
