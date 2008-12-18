$:.unshift(File.join(File.dirname(__FILE__)))
require "poolparty"
require "poolpartycl"

module PoolParty
  module Capistrano
    
    $cap_clouds = {}
    
    def set_poolparty_file(file)
      load_pool file
      instance_eval <<-EOE
set :username, "#{PoolParty::Base.user}"
ssh_options[:forward_agent] = true      
      EOE
    end
    
    def set_cloud(name)
      cld = PoolParty::Cloud.cloud(name)
      if cld
        $cap_clouds[name] = cld
        @cloud = cld
      end
      cld
    end
    
    def get_cloud(name)
      cld = set_cloud(name)
      instance_eval <<-EOE
ssh_options[:keys] = [ '#{cld.full_keypair_basename_path}' ]
set :user, '#{cld.user}'
EOE
      cld
    end
    
    def cloud_master(name)
      get_cloud(name).ip
    end
        
  end
end

module Capistrano
  class Configuration
    include ::PoolParty::Capistrano
        
    # Dir["#{::File.dirname(__FILE__)}/capistrano/*.rb"].each {|f| pload f }
  end
end