require File.dirname(__FILE__) + "/plugin_model"
require File.dirname(__FILE__) + "/resource"

module PoolParty    
  module Cloud
    def cloud(name=:main, &block)
      clouds.has_key?(name) ? clouds[name] : (clouds[name] = Cloud.new(name, self, &block))
    end

    def clouds
      $clouds ||= {}
    end
    
    def with_cloud(cl, opts={}, &block)
      cl.options.merge!(opts)
      cl.instance_eval &block if block
    end
    
    class Cloud
      attr_reader :name, :templates
      include PoolParty::PluginModel
      include PoolParty::Resources      
      include PrettyPrinter
      include Configurable
      include CloudResourcer
      # extend CloudResourcer
      # Net methods
      include PoolParty::Remote::RemoterBase
      include Remote
      
      default_options({
        :minimum_instances => 2,
        :maximum_instances => 4,
        :access_key => Base.access_key,
        :secret_access_key => Base.secret_access_key,
        :ec2_dir => ENV["EC2_HOME"],
        :keypair => (ENV["KEYPAIR_NAME"].nil? || ENV["KEYPAIR_NAME"].empty?) ? nil : ENV["KEYPAIR_NAME"],
        :ami => 'ami-44bd592d',
        :polling_time => "30.seconds"
      })
      
      def initialize(name, parent, &block)
        @name = name
        # store_block(&block)
        set_parent(parent) if parent
        self.instance_eval &block if block
        # this can be overridden in the spec, but ec2 is the default
        self.using :ec2
      end
                        
      # Keypairs
      # If the parent (pool) doesn't have a keypair defined on it, then generate one based on the 
      # pool_name and the cloud_name
      def keypair(*args)
        has_keypair? ? options[:keypair] : generate_keypair(*args)
      end
      def has_keypair?
        options.has_key?(:keypair) && options[:keypair]
      end
      def generate_keypair(*args)
        options[:keypair] = args.length > 0 ? args[0] : "#{@parent.respond_to?(:name) ? @parent.name : ""}_#{@name}"
      end
      
      # Prepare to send the new configuration to the instances
      # First, let's make sure that our base directory is made
      # Then copy the templates that have no other reference in
      # a spec file. Make sure the keys are stored in a file
      # For the master to have access to them
      # Then, send the saved containing cloud instances to give the 
      # remote master access to the cloud options that are required
      # for the master to run checks
      def prepare_to_configuration        
        # clear_base_directory
        make_base_directory
        copy_misc_templates
        Base.store_keys_in_file
        Script.save!
        copy_ssh_key # not my favorite...
      end
      
      def copy_ssh_key
        copy_file_to_storage_directory(full_keypair_path)
      end
      
      # Build the new poolparty manifest
      # Wrapping all of these requirements into the one 
      # poolparty class.
      # 
      # TODO: Consider the benefits of moving all the manifest
      # classes to separate files and keeping the containing
      # references in the include
      def build_and_store_new_config_file
        @manifest = build_manifest
        config_file = ::File.join(Base.storage_directory, "poolparty.pp")
        ::File.open(config_file, "w+") do |file|
          file << "class poolparty {"
          file << @manifest
          file << "}"
        end
      end      
      
      def copy_misc_templates
        ["fileserver.conf", "namespaceauth.conf"].each do |f|
          copy_file_to_storage_directory(::File.join(::File.dirname(__FILE__), "..", "templates", f))
        end
      end
      
      # Configuration files
      def build_manifest
        @build_manifest ||= build_from_existing_file
        unless @build_manifest
          reset_resources!
          add_poolparty_base_requirements
          
          @build_manifest = returning Array.new do |str|

            str << resources_string_from_resources(resources)

            # Refactor this into the resources method
            # TODO
            services.each do |service|
              @cp = classpackage_with_self(service)
              str << @cp.to_string
              str << @cp.include_string
            end

            str << "# Custom functions"
            str << Resources::CustomResource.custom_functions_to_string
          end.join("\n")          
        end
        @build_manifest
      end
      
      def build_from_existing_file
        if ::FileTest.file?("/etc/puppet/manifests/classes/poolparty.pp")
          open("/etc/puppet/manifests/classes/poolparty.pp").read
        else
          nil
        end
      end
      
      # To allow the remote instances to do their job,
      # they need a few options to run, these are the required options
      # to be saved on the remote "master" machine
      def minimum_runnable_options
        [:keypair, :minimum_instances, :maximum_instances]
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
        poolparty
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