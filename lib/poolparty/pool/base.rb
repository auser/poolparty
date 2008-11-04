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
      :tmp_path => "/tmp/poolparty",
      :remote_storage_path => "/var/poolparty",
      :remote_gem_path => "/var/poolparty/gems",
      :fileserver_base => "puppet://master/files",
      :base_config_directory => "/etc/poolparty",
      :template_directory => "templates",
      :template_path => "/var/lib/puppet/templates",
      :module_path => "/etc/puppet/modules/poolparty",
      :default_specfile_name => "pool.spec",
      :port => "80",
      :forwarding_port => "8080",
      :proxy_mode => "http",      
      # EC2 Options
      :ami => "ami-1cd73375" 
    })
        
    # Class methods
    class << self
      def options(h={})
        @options ||= default_options.merge(h)
      end
      # Get the access_key
      def access_key
        ENV["AWS_ACCESS_KEY_ID"] ? ENV["AWS_ACCESS_KEY_ID"] : load_keys_from_file[:access_key]
      end
      def secret_access_key
        ENV["AWS_SECRET_ACCESS_ID"] ? ENV["AWS_SECRET_ACCESS_ID"] : load_keys_from_file[:secret_access_key]
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
          dir if ::File.directory?(dir) && ::File.readable?(dir)
        end.first || ::File.join( "/tmp/poolparty")
      end
      
      def logger_location
        [
            "/var/log/poolparty"
        ].select do |dir|
          dir if ::File.directory?(dir) && ::File.readable?(dir)
        end.first || ::File.join(Dir.pwd, "log")
      end
      
      def pool_logger_location
        ::File.join(logger_location, "pool.logs")
      end
      
      # Array of allowed_commands that you can run on the remote nodes
      def allowed_commands
        @allowed_commands ||= open(::File.join( ::File.dirname(__FILE__), "..", "config", "allowed_commands.yml")).read.split(/\n/).map {|a| a.chomp }
      end
      
    end
  end    
end