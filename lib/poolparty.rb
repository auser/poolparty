require 'rubygems'

# Load required gems
%w(activesupport ftools logging ruby2ruby aska).each do |lib|
  begin
    require lib
  rescue Exception => e
    puts "Could not find library #{lib}: #{e}"
  end
  
end

# Use active supports auto load mechanism
ActiveSupport::Dependencies.load_paths << File.dirname(__FILE__)

## Load PoolParty
require "#{File.dirname(__FILE__)}/poolparty/version"

%w(core modules exceptions dependency_resolutions monitors net).each do |dir|
  Dir[File.dirname(__FILE__) + "/poolparty/#{dir}/**.rb"].each do |file|
    require file
  end
end

Kernel.load_p File.dirname(__FILE__) + "/poolparty/pool"

module PoolParty
  include FileWriter
  
  def logger
    @pool_logger ||= make_new_logger
  end
  
  private
  #:nodoc:#
  def make_new_logger
    FileUtils.mkdir_p Base.pool_logger_location unless ::File.directory?(Base.pool_logger_location)
    Loggable.new
  end
end

class Object
  include PoolParty
  include PoolParty::Pool
  include PoolParty::Cloud
  
  include PoolParty::DefinableResource
end

class Class
  include PoolParty::PluginModel  
end

## Load PoolParty Plugins and package
%w(plugins base_packages).each do |dir|
  Dir[File.dirname(__FILE__) + "/poolparty/#{dir}/**.rb"].each do |file|
    require file
  end
end