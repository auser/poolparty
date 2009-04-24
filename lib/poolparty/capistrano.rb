$:.unshift(File.join(File.dirname(__FILE__)))
require "poolparty"
require "poolpartycl"

module PoolParty
  module Capistrano
    
    $cap_clouds = {}
    
    def set_poolparty_file(file)
      load_pool file
    end
    
    def set_cloud(name)
      cld = PoolParty::Cloud.cloud(name)
      if cld && !cloud_retrieved_already?
        $cap_clouds[name] = cld
        @cloud = cld
        instance_eval <<-EOE
  ssh_options[:keys] = [ '#{cld.full_keypair_path}' ]
  set :user, '#{cld.user}'
  set :username, "#{cld.user}"
  ssh_options[:forward_agent] = true
  EOE
      end
      cld
    end
    
    def cloud_retrieved_already?
      $cap_clouds.key?(name)
    end
    def get_cloud(name)
      set_cloud(name)
    end
    
    # Get the master ip for the cloud to set in the deploy.rb file
    def cloud_master(name)
      get_cloud(name).ip
    end
    
    def set_role_for_all_instances(role, name)
      ips = get_cloud(name).nodes(:status => "running").map {|ri| ri.ip }
      ips.each {|ip| send :role, role.to_sym, "#{ip}"}
    end
  end
end

module Capistrano
  class Configuration
    include ::PoolParty::Capistrano
        
    # Dir["#{::File.dirname(__FILE__)}/capistrano/*.rb"].each {|f| pload f }
  end
end