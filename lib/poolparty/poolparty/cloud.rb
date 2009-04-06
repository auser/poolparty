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
      attr_reader :templates, :cloud_name

      include CloudResourcer
      include PoolParty::PluginModel
      include PoolParty::Resources      
      include PoolParty::DependencyResolverCloudExtensions
      include PrettyPrinter
      include Provisioner

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
      
      def method_missing(m, *args, &block)
        remote_base.respond_to?(m) ? remote_base.send(m, *args, &block) : super
      end
      
      default_options(
        :minimum_instances => 2,
        :maximum_instances => 5,
        :contract_when => "load < 0.65",
        :expand_when => "load > 1.9",
        :access_key => Default.access_key,
        :secret_access_key => Default.secret_access_key,
        :ec2_dir => ENV["EC2_HOME"],
        :minimum_runtime => Default.minimum_runtime,
        :user => Default.user
      )
      
      def initialize(name, &block)
        @cloud_name = name
        @cloud_name.freeze
        plugin_directory "#{pool_specfile ? ::File.dirname(pool_specfile) : Dir.pwd}/plugins"
        super        
        
        after_create
      end
      
      def name(*args)
        @cloud_name ||= @cloud_name ? @cloud_name : (args.empty? ? :default_cloud : args.first)
      end
      
      # Callback
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
      
      def setup_defaults
        # this can be overridden in the spec, but ec2 is the default
        using :ec2
        options[:keypair] = keypair.basename
        options[:rules] = {:expand => expand_when, :contract => contract_when}
        dependency_resolver 'chef'
      end
      
      # provide list of public ips to get into the cloud
      def ips
        list_of_running_instances.map {|ri| ri.ip }
      end
      
      def ip
        ips.first  #TODO: make this be a random ip, since we should not rely on it being the same each time
      end
      
      # TODO: Deprecated
      def dependency_resolver_command
         "/usr/bin/puppet -v --logdest syslog /etc/puppet/manifests/site.pp"
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
        copy_custom_templates
        store_keys_in_file
        # Script.save!(self)
        # not my favorite...
        copy_ssh_key
        write_unique_cookie
        before_configuration_tasks
        write_properties_hash if debugging || testing
      end
      
      def copy_custom_templates
        return true unless ::File.directory?("#{Dir.pwd}/templates")
        Dir["#{Dir.pwd}/templates/*"].each {|file| copy_template_to_storage_directory(file, true) }        
      end
      
      # Copy the ssh keys to the storage directory in preparation for
      # configuration
      def copy_ssh_key
        copy_file_to_storage_directory(full_keypair_path)
      end
      
      # Store our keys for cloud access in a file 
      # that is specific to this cloud
      def store_keys_in_file
        Default.store_keys_in_file_for(self)
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
        Digest::SHA256.hexdigest("#{keypair.basename}#{name}")[0..12]
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
      
      def copy_misc_templates
        ["namespaceauth.conf", "puppet/puppet.conf", "gem"].each do |f|
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
        unless Default.custom_monitor_directories.empty?
          make_directory_in_storage_directory("monitors")
          Default.custom_monitor_directories.each do |dir|
            Dir["#{dir}/*.rb"].each {|f| copy_file_to_storage_directory(f, "monitors")}
          end
        end
      end
      
      def copy_custom_modules
        unless Default.custom_modules_directories.empty?
          make_directory_in_storage_directory("modules")
          Default.custom_modules_directories.each do |dir|
            Dir["#{dir}/*"].each do |d|
              to = ::File.join("modules", ::File.basename(d))
              copy_directory_into_storage_directory(d, to) if ::File.directory?(d)
            end
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
      
      #FIXME DEPRECATE
      # def build_short_manifest
      #               returning Array.new do |str|            
      #         
      #                 # Refactor this into the resources method
      #                 # TODO
      #                 services.each do |service|
      #                   service.options.merge!(:name => service.name)
      #                   classpackage_with_self(service)
      #                 end
      #                 
      #                 options.merge!(:name => "user")
      #                 classpackage_with_self
      #                 # resources.each do |type, res|
      #                 #   str << "# #{type.to_s.pluralize}"
      #                 #   str << res.to_string
      #                 # end
      #                 
      #                 global_classpackages.each do |cls|
      #                   str << cls.to_string
      #                 end
      #         
      #                 str << "# Custom functions"
      #                 str << Resources::CustomResource.custom_functions_to_string
      #               end.join("\n")
      #             end
      
      def build_from_existing_file
        ::FileTest.file?("#{Default.manifest_path}/classes/poolparty.pp") ? open("#{Default.manifest_path}/classes/poolparty.pp").read : nil
      end
      
      def write_properties_hash(filename=::File.join(Default.tmp_path, Default.properties_hash_filename) )
        file_path = ::File.dirname(filename)
        file_name = "#{::File.basename(filename, ::File.extname(filename))}_#{name}#{::File.extname(filename)}"
        output = to_properties_hash.to_json
        ::File.open("#{file_path}/#{file_name}", "w") {|f| f.write output }
        true
      end
      
      # To allow the remote instances to do their job,
      # they need a few options to run, these are the required options
      # to be saved on the remote "master" machine
      def minimum_runnable_options
        ([
          :keypair, :minimum_instances, :maximum_instances,
          :expand_when, :contract_when, :set_master_ip_to  #DEPRECATE set_master_ip_to
        ]<< custom_minimum_runnable_options).flatten
      end
      
      def custom_minimum_runnable_options
        using_remoter? ? remote_base.custom_minimum_runnable_options : []
      end
      
      def remote_base
        @remote_base ||= nil
      end
            
      # Callbacks on before_bootstrap
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
        returning Array.new do |arr|
          clouds.each do |name, cl|
            arr << cl if name != self.name
          end
        end
      end
      
      def reset!
        reset_remoter_base!
        @build_manifest = @describe_instances = @remote_instances_list = nil
      end            
    end
  end 
end
