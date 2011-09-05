$LOAD_PATH.unshift(File.dirname(__FILE__))

t=Time.now

require 'yaml'

# Load system gems
%w(rubygems logger erb open-uri fileutils timeout).each do |lib|
  require lib
end

begin
  require 'AWS'
rescue LoadError
  puts <<-EOM
  There was an error requiring AWS
EOM
end

require 'pp'

# Add all vendor gems to the load paths
Dir[File.dirname(__FILE__)+"/../vendor/gems/*"].each {|lib| $LOAD_PATH.unshift(File.expand_path("#{lib}/lib")) }

# Load local gems
%w(dslify json searchable_paths).each do |dep|
  require dep
end

require "poolparty/version"
module PoolParty
  def self.version
    VERSION
  end

  def self.lib_dir
    File.join(File.dirname(__FILE__), "..")
  end
end

# Require the poolparty error so we can use it ubiquitously
require "poolparty/pool_party_error"

# Core object overloads
%w( object
    string
    array
    hash
    symbol
  ).each do |lib|
  require "core/#{lib}"
end

require "keypair"

POOLPARTY_CONFIG_FILE = "#{ENV["HOME"]}/.poolparty/aws" unless defined?(POOLPARTY_CONFIG_FILE)

# PoolParty core
$LOAD_PATH.unshift(File.dirname(__FILE__)/"poolparty")
%w( base
    chef_attribute
    chef
    chef_solo
    chef_client
    cloud pool
  ).each do |lib|
  require "poolparty/#{lib}"
end

require 'cloud_providers'

puts "PoolParty core loadtime: #{Time.now-t}"
