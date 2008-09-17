require 'rubygems'

# Load required gems
require 'active_support'
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
end

class Object
  include PoolParty
  include PoolParty::Pool
end

class Class
  include PoolParty::PluginModel
end