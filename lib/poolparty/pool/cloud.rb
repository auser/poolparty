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
    
    def with_cloud(cloud, opts={}, &block)
      cloud.options.merge!(opts)
      cloud.instance_eval &block if block
    end
    
    class Cloud
      attr_reader :name, :templates
      include PoolParty::PluginModel
      include PoolParty::Resources
      include Configurable
      include PrettyPrinter
      include CloudResourcer
      # Net methods
      include PoolParty::Remote::RemoterBase
      include Remote
      
      default_options({
        :minimum_instances => 2,
        :maximum_instances => 4,
        :access_key => ENV["AWS_ACCESS_KEY"],
        :secret_access_key => ENV["AWS_SECRET_ACCESS"],
        :ec2_dir => ENV["EC2_HOME"],
        :keypair => (ENV["KEYPAIR_NAME"].nil? || ENV["KEYPAIR_NAME"].empty?) ? nil : ENV["KEYPAIR_NAME"],
        :ami => 'ami-44bd592d',
        :polling_time => "30.seconds"
      })
      
      def initialize(name, parent, &block)
        @name = name        
        set_parent(parent) if parent
        self.instance_eval &block if block_given?
        # this can be overridden in the spec, but ec2 is the default
        using :ec2
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
      
      def prepare_to_configuration
        make_base_directory
        # clear_base_directory
        copy_misc_templates
      end
      
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
        reset_resources!        
        add_poolparty_base_requirements
        
        returning Array.new do |str|
          
          str << resources_string
          
          services.each do |service|
            str << "# #{service.name}\n"
            str << "class #{service.name} {\n"
            str << service.resources_string("\t\t")
            str << "}\n"
            str << "include #{service.name}"
          end
          
          str << "# Custom functions"
          str << Resources::CustomResource.custom_functions_to_string
          str << "\n"
        end.join("\n")
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