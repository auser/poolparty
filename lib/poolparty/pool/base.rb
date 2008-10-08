=begin rdoc
  Base
  This handles user interaction, loading the parameters, etc.
=end
require "open-uri"
require "ftools"
module PoolParty
  class Base
    include Configurable
    extend MethodMissingSugar
    
    default_options({
      :environment => "production",
      :user => "root", # This should change here
      :base_keypair_path => "~/.ec2",
      :tmp_path => "tmp",
      :remote_storage_path => "/var/poolparty",
      :storage_directory => File.join(Dir.pwd, "tmp"),
      :fileserver_base => "puppet:///files",
      :base_config_directory => "/etc/poolparty",
      :template_directory => "tmp/templates",
      :template_path => "/var/lib/puppet/templates",
      :port => "80",
      :forwarding_port => "8080",
      :proxy_mode => "http",
      :pool_logger_location => File.join(Dir.pwd, "logs"),
      # EC2 Options
      :ami => "ami-4bb05422" 
    })
        
    # Class methods
    class << self
      def actionable_default_options
        default_options.merge!({:access_key => self.access_key,:secret_access_key => secret_access_key})
      end
      def options(h={})
        @options ||= actionable_default_options.merge(h)
      end
      # Get the access_key
      def access_key
        ENV["AWS_ACCESS_KEY_ID"] ? ENV["AWS_ACCESS_KEY_ID"] : load_keys_from_file[:access_key]
      end
      def secret_access_key
        ENV["AWS_SECRET_ACCESS_ID"] ? ENV["AWS_SECRET_ACCESS_ID"] : load_keys_from_file[:secret_access_key]
      end
      def load_keys_from_file
        @keys ||= get_working_key_file_locations ? YAML::load( open(get_working_key_file_locations).read ) : {}
      end
      def store_keys_in_file
        unless access_key.nil? || secret_access_key.nil?
          write_to_file( key_file_locations.first, YAML::dump({:access_key => access_key, :secret_access_key => secret_access_key}))        
        end
      end
      def reset!
        @keys = nil
      end
      # Get the instance first instance file that exists on the system from the expected places
      # denoted in the local_instances_list_file_locations
      def get_working_key_file_locations
        key_file_locations.reject {|f| f unless ::File.file?(f) }.first
      end
      # Expected places for the instances.list to be located at on the machine
      def key_file_locations
        [
          "#{Base.storage_directory}/.ppkeys",
          "#{Base.base_config_directory}/.ppkeys",
          "~/.ppkeys",
          "ppkeys"
        ]
      end
      
    end
  end    
end