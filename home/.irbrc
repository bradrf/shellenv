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

puts "(loaded #{__FILE__})"