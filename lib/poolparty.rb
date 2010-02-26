$LOAD_PATH.unshift(File.dirname(__FILE__))

t=Time.now

# Load system gems
%w(rubygems logger erb open-uri).each do |lib|
  require lib
end

# Add all vendor gems to the load paths
Dir[File.dirname(__FILE__)+"/../vendor/gems/*"].each {|lib| $LOAD_PATH.unshift(File.expand_path("#{lib}/lib")) }

# Load local gems
%w(dslify json searchable_paths).each do |dep|
  require dep
end

module PoolParty
  def self.version
    return @version if @version
    config = YAML.load(File.read(File.expand_path("#{File.dirname(__FILE__)}/../VERSION.yml")))
    @version = "#{config[:major]}.#{config[:minor]}.#{config[:patch]}"
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

# PoolParty core
$LOAD_PATH.unshift(File.dirname(__FILE__)/"poolparty")
%w( base 
    chef_attribute
    chef
    chef_solo
    cloud pool
  ).each do |lib|
  require "poolparty/#{lib}"
end

require 'cloud_providers'

puts "PoolParty core loadtime: #{Time.now-t}"
