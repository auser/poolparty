require File.dirname(__FILE__) + "/plugin_model"
require File.dirname(__FILE__) + "/resource"

module PoolParty    
  module Cloud
    def cloud(name=:app, parent=self, &block)
      clouds.has_key?(name) ? clouds[name] : (clouds[name] = Cloud.new(name, parent, &block))
    end

    def clouds
      $clouds ||= {}
    end
    
    def with_cloud(cl, opts={}, &block)
      raise CloudNotFoundException.new("Cloud not found") unless cl
      cl.options.merge!(opts) if opts
      cl.run_in_context &block if block
    end
    
    class Cloud
      attr_reader :templates
      include PoolParty::PluginModel
      include PoolParty::Resources      
      include PrettyPrinter
      include Configurable
      include CloudResourcer
      include Provisioner
      # extend CloudResourcer
      # Net methods      
      include Remote
      include PoolParty::CloudDsl
      
      default_options({
        :minimum_instances => 2,
        :maximum_instances => 5,
        :contract_when => "cpu < 0.65",
        :expand_when => "cpu > 1.9",
        :access_key => Base.access_key,
        :secret_access_key => Base.secret_access_key,
        :ec2_dir => ENV["EC2_HOME"],
        :keypair => (ENV["KEYPAIR_NAME"].nil? || ENV["KEYPAIR_NAME"].empty?) ? nil : ENV["KEYPAIR_NAME"],
        :minimum_runtime => Base.minimum_runtime,
        :user => Base.user,
        :ami => 'ami-44bd592d'
      })
      
      def initialize(name, pare=self, &block)
        @cloud_name = name
        @cloud_name.freeze
                
        plugin_directory
                
        p = pare.is_a?(PoolParty::Pool::Pool) ? pare : nil
        store_block(&block)
        run_setup(p, &block)        
        
        # set_parent(parent) if parent && !@parent
        # self.run_in_context parent, &block if block
        setup_defaults
        # realize_plugins!
        # reset! # reset the clouds
        # reset_remoter_base!
      end
      
      def setup_defaults
        # this can be overridden in the spec, but ec2 is the default
        self.using :ec2
        generate_keypair unless has_keypair?
      end
      
      def name
        @cloud_name
      end
            
      # Prepare to send the new configuration to the instances
      # First, let's make sure that our base directory is made
      # Then copy the templates that have no other reference in
      # a spec file. Make sure the keys are stored in a file
      # For the master to have access to them
      # Then, send the saved containing cloud instances to give the 
      # remote master access to the cloud options that are required
      # for the master to run checks
      def prepare_for_configuration        
        # clear_base_directory
        make_base_directory
        copy_misc_templates
        copy_custom_monitors
        copy_custom_modules
        store_keys_in_file
        Script.save!(self)
        # not my favorite...
        copy_ssh_key
        write_unique_cookie
        before_configuration_tasks
      end
      
      # Copy the ssh keys to the storage directory in preparation for
      # configuration
      def copy_ssh_key
        copy_file_to_storage_directory(full_keypair_path)
      end
      
      # Store our keys for cloud access in a file 
      # that is specific to this cloud
      def store_keys_in_file
        Base.store_keys_in_file_for(self)
      end
      
      # Let's write the cookie into the tmp path
      def write_unique_cookie
        write_to_file_in_storage_directory("cookie") do
          generate_unique_cookie_string
        end
      end
      
      # Generate a unique cookie string so that our erlang modules can 
      # talk to each other safely. This is based off the keypair
      # and the name of the cloud
      def generate_unique_cookie_string
        Digest::SHA256.hexdigest("#{full_keypair_name}#{name}")[0..12]
      end
      
      # Build the new poolparty manifest
      # Wrapping all of these requirements into the one 
      # poolparty class.
      # 
      # TODO: Consider the benefits of moving all the manifest
      # classes to separate files and keeping the containing
      # references in the include
      def build_and_store_new_config_file(force=false)
        vputs "Building new manifest configuration file (forced: #{force})"
        manifest = force ? rebuild_manifest : build_manifest
        config_file = ::File.join(Base.storage_directory, "poolparty.pp")
        ::File.open(config_file, "w") do |file|
          file << manifest
        end
      end      
      
      def copy_misc_templates
        ["namespaceauth.conf", "puppet.conf", "gem"].each do |f|
          copy_file_to_storage_directory(::File.join(::File.dirname(__FILE__), "..", "templates", f))
        end
      end
      
      # If there is a directory named monitors in the same directory
      # as the pool specification file is in,
      # then create a monitors directory in the storage directory
      # and mirror the two. When PoolParty "boots" up, it scans
      # the monitors directory for any custom monitors
      # that are in known locations, these are included
      def copy_custom_monitors
        unless Base.custom_monitor_directories.empty?
          make_directory_in_storage_directory("monitors")
          Base.custom_monitor_directories.each do |dir|
            Dir["#{dir}/*.rb"].each {|f| copy_file_to_storage_directory(f, "monitors")}
          end
        end        
      end
      
      def copy_custom_modules
        unless Base.custom_modules_directories.empty?
          make_directory_in_storage_directory("modules")
          Base.custom_modules_directories.each do |dir|
            Dir["#{dir}/*"].each do |d|
              to = ::File.join("modules", ::File.basename(d))
              copy_directory_into_storage_directory(d, to) if ::File.directory?(d)
            end
          end
        end
      end
            
      # Configuration files
      def build_manifest
        vputs "Building manifest"
        @build_manifest ||= build_from_existing_file
        unless @build_manifest
          
          add_poolparty_base_requirements
          
          @build_manifest = "class poolparty {\n #{build_short_manifest}\n}"
        end
        @build_manifest
      end
      
      def rebuild_manifest
        @build_manifest = nil
        build_manifest
      end
      
      def build_short_manifest
        returning Array.new do |str|            

          # Refactor this into the resources method
          # TODO
          services.each do |service|
            service.options.merge!(:name => service.name)
            classpackage_with_self(service)
          end
          
          options.merge!(:name => "user")
          classpackage_with_self
          # resources.each do |type, res|
          #   str << "# #{type.to_s.pluralize}"
          #   str << res.to_string
          # end
          
          global_classpackages.each do |cls|
            str << cls.to_string
          end

          str << "# Custom functions"
          str << Resources::CustomResource.custom_functions_to_string
        end.join("\n")
      end
      
      def build_from_existing_file
        ::FileTest.file?("#{Base.manifest_path}/classes/poolparty.pp") ? open("#{Base.manifest_path}/classes/poolparty.pp").read : nil
      end
      
      # To allow the remote instances to do their job,
      # they need a few options to run, these are the required options
      # to be saved on the remote "master" machine
      def minimum_runnable_options
        ([
          :keypair, :minimum_instances, :maximum_instances,
          :expand_when, :contract_when, :set_master_ip_to
        ]<< custom_minimum_runnable_options).flatten
      end
      
      # Add all the poolparty requirements here
      # NOTE: These are written as plugins in the lib/poolparty/base_packages directory
      # for examples. 
      # Also note that there is no block associated. This is because we have written
      # all that is necessary in a method called enable
      # which is called when there is no block
      def add_poolparty_base_requirements
        heartbeat
        haproxy
        ruby
        poolparty_base_packages
        realize_plugins!(true) # Force realizing of the plugins
      end
      
      def other_clouds
        returning Array.new do |arr|
          clouds.each do |name, cl|
            arr << cl if name != self.name
          end
        end
      end
      
      def reset!
        reset_remoter_base!
        @build_manifest = @describe_instances = nil
      end
            
      # Add to the services pool for the manifest listing
      def add_service(serv)
        services << serv
      end
      # Container for the services
      def services
        @services ||= []
      end
            
    end
  end  
end