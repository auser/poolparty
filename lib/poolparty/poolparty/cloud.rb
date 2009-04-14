require File.dirname(__FILE__) + "/plugin_model"
require File.dirname(__FILE__) + "/resource"

module PoolParty    
  module Cloud
    def cloud(name=:app, &block)
      clouds[name] ||= Cloud.new(name, &block)
    end

    def clouds
      $clouds ||= {}
    end
    
    def with_cloud(cl, opts={}, &block)
      raise CloudNotFoundException.new("Cloud not found") unless cl
      cl.options.merge!(opts) if opts
      cl.run_in_context &block if block
    end
    
    class Cloud < PoolParty::PoolPartyBaseClass
      attr_reader :templates, :cloud_name, :remote_base

      include CloudResourcer
      include PoolParty::PluginModel
      include PoolParty::Resources      
      include PoolParty::DependencyResolverCloudExtensions
      include PrettyPrinter

      # Net methods
      include ::PoolParty::Remote
      include PoolParty::CloudDsl
      include PoolParty::Monitors

      def verbose
        true
      end
      
      def self.immutable_methods
        [:name]
      end
      
      def self.method_added sym        
        raise "Exception: #{sym.to_s.capitalize} method has been redefined" if immutable_methods.include?(sym) && !respond_to?(sym)
      end      
      
      alias :name :cloud_name
      
      # Call the remoter commands on the cloud if they don't exist on the cloud itself
      # This gives the cloud access to the remote_base's methods
      def method_missing(m, *args, &block)
        remote_base.respond_to?(m) ? remote_base.send(m, *args, &block) : super
      end
      
      default_options(
        :minimum_instances => 2,
        :maximum_instances => 5,
        :access_key => Default.access_key,
        :secret_access_key => Default.secret_access_key,
        :ec2_dir => ENV["EC2_HOME"],
        :minimum_runtime => Default.minimum_runtime,
        :user => Default.user
      )
      
      # Freeze the cloud_name so we can't modify it at all, set the plugin_directory
      # call and run instance_eval on the block and then call the after_create callback
      def initialize(name, &block)
        @cloud_name = name
        @cloud_name.freeze
        plugin_directory "#{pool_specfile ? ::File.dirname(pool_specfile) : Dir.pwd}/plugins"
        super        
        
        after_create
      end
      
      # Fetch the name of the cloud
      def name(*args)
        @cloud_name ||= @cloud_name ? @cloud_name : (args.empty? ? :default_cloud : args.first)
      end
      
      # Callback
      # called after the cloud has been created, everything has run and is set at this point
      # here the base requirements are added as well as an empty chef recipe is called
      # Also, the after_create hook on the plugins used by the cloud are called here
      def after_create
        dputs "In after create"
        ::FileUtils.mkdir_p("#{Default.tmp_path}/dr_configure")
        run_in_context do
          add_poolparty_base_requirements
          chef do
          end
        end
        plugin_store.each {|a| a.after_create }
        setup_defaults
      end
      
      # setup defaults for the cloud
      def setup_defaults
        # this can be overridden in the spec, but ec2 is the default
        using :ec2
        options[:keypair] ||= keypair.basename rescue nil
        options[:rules] = {:expand => expand_when, :contract => contract_when}
        dependency_resolver 'chef'
      end
      
      # provide list of public ips to get into the cloud
      def ips
        list_of_running_instances.map {|ri| ri.ip }
      end
      
      # TODO: make this be a random ip, since we should not rely on it being the same each time
      def ip
        ips.first
      end
            
      # Build the new poolparty manifest
      # Wrapping all of these requirements into the one 
      # poolparty class.
      # 
      # TODO: Consider the benefits of moving all the manifest
      # classes to separate files and keeping the containing
      # references in the include
      def build_and_store_new_config_file(filepath=nil, force=false)
        filepath ||= ::File.join(Default.storage_directory, "poolparty.pp")
        # write_properties_hash if debugging
        vputs "Building new manifest configuration file (forced: #{force})"
        manifest = force ? rebuild_manifest : build_manifest
        ::File.open(filepath, "w") do |file|
          file << manifest
        end
      end
      
      # If there is a directory named monitors in the same directory
      # as the pool specification file is in,
      # then create a monitors directory in the storage directory
      # and mirror the two. When PoolParty "boots" up, it scans
      # the monitors directory for any custom monitors
      # that are in known locations, these are included
      def copy_custom_monitors
        unless Default.custom_monitor_directories.empty?
          Default.custom_monitor_directories.each do |dir|
            Dir["#{dir}/*.rb"].each {|f| ::Suitcase::Zipper.add(f, "monitors")}
          end
        end
      end

      #FIXME MOVE TO DEPENDECY RESOL
      # Configuration files
      def build_manifest
        vputs "Building manifest"
        @build_manifest ||= build_from_existing_file
        unless @build_manifest          
          props = to_properties_hash
         
          @build_manifest =  options[:dependency_resolver].send(:compile, props, self)
        end
        @build_manifest
      end
      
      def rebuild_manifest
        @build_manifest = nil
        build_manifest
      end
      
      # If the 
      def build_from_existing_file
        ::FileTest.file?("#{Default.base_config_directory}/poolparty.pp") ? open("#{Default.base_config_directory}/poolparty.pp").read : nil
      end
      
      def write_properties_hash(filename=::File.join(Default.tmp_path, Default.properties_hash_filename) )
        file_path = ::File.dirname(filename)
        file_name = "#{::File.basename(filename, ::File.extname(filename))}_#{name}#{::File.extname(filename)}"
        output = to_properties_hash.to_json
        ::File.open("#{file_path}/#{file_name}", "w") {|f| f.write output }
        true
      end
            
      # Callbacks on bootstrap and configuration
      %w( before_bootstrap 
          after_bootstrap 
          before_configure 
          after_configure).each do |meth|
        module_eval <<-EOE
          def call_#{meth}_callbacks
            plugin_store.each {|a| a.#{meth} }
          end
        EOE
      end
      
      # Add all the poolparty requirements here
      # NOTE: These are written as plugins in the lib/poolparty/base_packages directory
      # for examples. 
      # Also note that there is no block associated. This is because we have written
      # all that is necessary in a method called enable
      # which is called when there is no block
      def add_poolparty_base_requirements
        poolparty_base_haproxy
        poolparty_base_heartbeat
        poolparty_base_ruby
        poolparty_base_packages
      end
      
      def other_clouds
        arr = []
        clouds.each do |name, cl|
          arr << cl if name != self.name
        end
        arr
      end
      
      def reset!
        reset_remoter_base!
        @build_manifest = @describe_instances = @remote_instances_list = nil
      end            
    end
  end 
end
