require 'rubygems'

# Load required gems
require 'active_support'
require 'open4'
require "backcall"

# Use active supports auto load mechanism
ActiveSupport::Dependencies.load_paths << File.dirname(__FILE__)

## Load PoolParty
%w(core modules exceptions net).each do |dir|
  Dir[File.dirname(__FILE__) + "/poolparty/#{dir}/**.rb"].each do |file|
    require file
  end
end

Kernel.load_p File.dirname(__FILE__) + "/poolparty/pool/**"

module PoolParty
  def copy_file_to_storage_directory(file)
    FileUtils.cp file, Base.storage_directory
  end
  def write_to_file_in_storage_directory(file, str)
    path = File.join(Base.storage_directory, file)
    FileUtils.mkdir_p File.dirname(path)
    File.open(path, "w+") do |f|
      f << str
    end
  end
end

class Object
  include PoolParty
  include PoolParty::Pool
  include PoolParty::Cloud
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