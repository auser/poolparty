require 'rubygems'
require 'active_support'

# Use active supports auto load mechanism
Dependencies.load_paths << File.dirname(__FILE__)

## Load PoolParty
%w(core modules).each do |dir|
  Dir[File.dirname(__FILE__) + "/poolparty/#{dir}/**"].each do |file|
    require file
  end
end

Kernel.load_p File.dirname(__FILE__) + "/poolparty/pool/**"

module PoolParty
end

class Object
  include PoolParty  
  include PoolParty::Cloud
  include PoolParty::Pool
end