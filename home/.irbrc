# -*- Mode: ruby -*-

require 'irb/completion'

require 'yaml'
require 'stringio'
require 'zlib'
require 'time'
require 'fileutils'

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
        if File.exists?(HISTFILE)
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
              @@models << (model = Kernel.const_get(name))
              model.module_exec do
                scope :like, ->(matcher) {
                  m = matcher.first
                  where arel_table[m[0]].matches("%#{m[1]}%")
                }
              end
            rescue Exception => ex
              $stderr.puts "*** #{ex}"
            end
          end
        end
        return @@models
      end
    end

    def list_models
      puts
      Rails.models.each do |model|
        model.new
        puts model.inspect
      end
      puts
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

puts "(loaded #{__FILE__})"
