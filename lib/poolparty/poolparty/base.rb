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
      :user => "root", # This should change here
      :base_keypair_path => "#{ENV["HOME"]}/.ec2",
      :tmp_path => "/tmp/poolparty",
      :remote_storage_path => "/var/poolparty",
      :remote_gem_path => "/var/poolparty/gems",
      :fileserver_base => "puppet://master/files",
      :base_config_directory => "/etc/poolparty",
      :template_directory => "templates",
      :template_path => "/var/lib/puppet/templates",
      :module_path => "/etc/puppet/modules/poolparty",
      :default_specfile_name => "clouds.pool",
      :default_project_specfile_name => "spec/clouds.pool",
      :port => "80",
      :forwarding_port => "8080",
      :proxy_mode => "http",
      :messenger_client_port => 7050,
      # EC2 Options
      :ami => "ami-1cd73375",
      :size => 'm1.small', # must be 'm1.small', 'm1.large', 'm1.xlarge', 'c1.medium', or 'c1.xlarge'
      :availabilty_zone => "us-east-1a",
      :security_group => ["default"],
      # Options that should not be touched pretty much ever
      :manifest_path => "/etc/puppet/manifests"
    })
        
    # Class methods
    class << self
      def options(h={})
        @options ||= default_options.merge(h)
      end
      # Get the access_key
      def access_key
        @access_key ||= ENV["AWS_ACCESS_KEY"] ? ENV["AWS_ACCESS_KEY"] : load_keys_from_file[:access_key]
      end
      def secret_access_key
        @secret_access_key ||= ENV["AWS_SECRET_ACCESS_KEY"] ? ENV["AWS_SECRET_ACCESS_KEY"] : load_keys_from_file[:secret_access_key]
      end
      def read_keyfile
        open(get_working_key_file_locations).read
      end
      def load_keys_from_file
        @keys ||= get_working_key_file_locations ? YAML::load( read_keyfile ) : {}
      end
      # Store the keys in a yaml format to give the master access
      # So that the master has access to the files
      def store_keys_in_file
        unless access_key.nil? || secret_access_key.nil?
          write_to_file( key_file_locations.first, YAML::dump({:access_key => access_key, :secret_access_key => secret_access_key}))        
        end
      end
      def store_keys_in_file_for(obj=nil)
        if obj
          @access_key = obj.access_key
          @secret_access_key = obj.secret_access_key
        end
        store_keys_in_file
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
          ".ppkeys",
          "#{Base.base_config_directory}/.ppkeys",
          "#{Base.storage_directory}/ppkeys",          
          "~/.ppkeys",
          "ppkeys"
        ]
      end
      def storage_directory
        [
            "/var/poolparty"           
        ].select do |dir|
          dir if viable_directory?(dir)
        end.first || ::File.join( "/tmp/poolparty")
      end
      def logger_location
        [
            "/var/log/poolparty"
        ].select do |dir|
          dir if viable_directory?(dir)
        end.first || ::File.join(Dir.pwd, "log")
      end
      # Assume the logs will be at the pool.log location within the 
      # logger_location set above
      def pool_logger_location
        ::File.join(logger_location, "pool.log")
      end
      def custom_monitor_directories
        [
          "/var/poolparty/monitors",
          "/etc/poolparty/monitors",
          "#{Dir.pwd}/monitors"
        ].select {|d| d if viable_directory?(d) }
      end
      # Only return true if the directory we are reading is both readable
      # and exists
      def viable_directory?(dir)
        ::File.directory?(dir) && ::File.readable?(dir)
      end
    end
  end    
end