=begin rdoc
  The Provisioner is responsible for provisioning REMOTE servers
  This class only comes in to play when calling the setup commands on
  the development machine
=end
require 'capistrano/cli'

module PoolParty
  module Provisioner
    
    # Provision master
    # Convenience method to clean 
    def self.provision_master(cloud, testing=false)
      Provisioner::Master.new(cloud).process_install!(testing)
    end

    def self.configure_master(cloud, testing=false)
      Provisioner::Master.new(cloud).process_configure!(testing)
    end
    
    def self.reconfigure_master(cloud, testing=false)
      Provisioner::Master.new(cloud).process_reconfigure!(testing)
    end

    def self.provision_slaves(cloud, testing=false)
      cloud.nonmaster_nonterminated_instances.each do |sl|
        provision_slave(sl, cloud, testing)
      end
    end

    def self.configure_slaves(cloud, testing=false)
      cloud.nonmaster_nonterminated_instances.each do |sl|
        configure_slave(sl, cloud, testing)
      end
    end
        
    def self.provision_slave(instance, cloud, testing=false)
      Provisioner::Slave.new(instance, cloud).process_install!(testing)
    end
    
    def self.configure_slave(instance, cloud, testing=false)
      Provisioner::Slave.new(instance, cloud).process_configure!(testing)
    end
    
    def self.become_master(cloud, testing=false)
      Provisioner::BecomeMaster.new(cloud).process_install!(testing)
    end
    
    def self.process_clean_reconfigure_for!(instance, cloud, testing=false)
      Provisioner::Master.new(cloud).process_clean_reconfigure_for!(instance, testing)
    end
    
    def self.clear_master_ssl_certs(cloud, testing=false)
      Provisioner::Master.new(cloud).clear_master_ssl_certs
    end
    
    class ProvisionerBase
      attr_accessor :config, :loaded_tasks
      
      include Configurable
      include CloudResourcer
      include FileWriter
      
      def initialize(instance,cld=self, os=:ubuntu)
        @instance = instance
        @cloud = cld
        
        options(cloud.options) if cloud && cloud.respond_to?(:options)
        # set_vars_from_options(instance.options) unless instance.nil? || !instance.options || !instance.options.empty?
        # options(instance.options) if instance.respond_to?(:options)
        
        @os = os.to_s.downcase.to_sym
        create_config
        
        loaded
      end
      # Create the config of Cap
      def create_config
        @config = ::Capistrano::Configuration.new
        @config.logger.level = verbose ? ::Capistrano::Logger::INFO : ::Capistrano::Logger::IMPORTANT
        @config.set(:password) { ::Capistrano::CLI.password_prompt }        
        @config.load @cloud.deploy_file if @cloud.deploy_file
      end
      # Callback after initialized
      def loaded(opts={}, parent=self)      
      end
      
      def loaded_tasks
        @loaded_tasks ||= []
      end
            
      ### Installation tasks
      
      # This is the actual runner for the installation    
      def install
        valid? ? install_string : error
      end
      # Write the installation tasks to a file in the storage directory
      def name
        @instance.name
      end
      # TODO: Clean up this method
      def process_install!(testing=false)
        error unless valid?
        setup_runner
        
        unless testing
          vputs "Logging on to #{@instance.ip} (#{@instance.name})"
          @cloud.rsync_storage_files_to(@instance)
          vputs "Preparing configuration on the master"
          
          before_install(@instance)
          
          # process_clean_reconfigure_for!(@instance, testing)
          
          vputs "Provisioning #{@instance.name}"
          # /bin/rm install_#{name}.sh
          # cmd = "cd #{Base.remote_storage_path} && /bin/chmod +x install_#{name}.sh && /bin/sh install_#{name}.sh"
          # verbose ? @cloud.run_command_on(cmd, @instance) : hide_output {@cloud.run_command_on(cmd, @instance)}
          do_it(:install)
          
          after_install(@instance)
        end
      end
      
      def run_cap(meth=:install)
        commands = meth == :install ? install_tasks : configure_tasks
        
        define_task(meth, roles) do
          via = fetch(:run_method, :sudo)
          commands.each do |command|
            invoke_command command, :via => via
          end
        end
        
        begin
          run(name)
          return true
        rescue ::Capistrano::CommandError => e
          return false unless verbose
          
          # Reraise error if we're not suppressing it
          raise
        end        
      end
      # Install callbacks
      # Before installation callback
      def before_install(instance)        
      end
      def after_install(instance)        
      end
      
      ### Configuraton tasks
      
      def configure
        valid? ? configure_string : error
      end
      def process_configure!(testing=false)
        error unless valid?
        write_configure_file
        setup_runner
        
        unless testing
          vputs "Logging on to #{@instance.ip}"
          @cloud.rsync_storage_files_to(@instance)
          #  && /bin/rm configure_#{name}.sh
          cmd = "cd #{Base.remote_storage_path} && /bin/chmod +x configure_#{name}.sh && /bin/sh configure_#{name}.sh"
          verbose ? @cloud.run_command_on(cmd, @instance) : hide_output {@cloud.run_command_on(cmd, @instance)}
        end
      end
      def process_reconfigure!(testing=false)
        @cloud.run_command_on(PoolParty::Remote::RemoteInstance.puppet_runner_command, @instance) unless testing
      end
      # Tasks that need to be performed everytime we do any
      # remote ssh'ing into any instance
      def setup_runner(force=false)
        @cloud.prepare_for_configuration
        @cloud.build_and_store_new_config_file(force)
      end
      def valid?
        true
      end
      def error
        "Error in installation"
      end
      def after_install_tasks
        []
      end
      def after_configure_tasks
        []
      end
      # Tasks with default tasks 
      # These are run on all the provisioners, master or slave
      def default_install_tasks
        [
          :first_install_tasks,
          :upgrade_system,
          :install_rubygems,
          :make_logger_directory,
          :install_puppet,
          :fix_rubygems,
          :setup_system_for_poolparty,
          :custom_install_tasks
        ] << install_tasks
      end
      # Tasks with default configuration tasks
      # This is run on the provisioner, regardless
      def default_configure_tasks
        [
          custom_configure_tasks
        ] << configure_tasks
      end
      # Build a list of the tasks to run on the instance
      def install_tasks(a=[])
        @install_task ||= a
      end
      # Set the first tasks to be called on the instance
      # before any other tasks are run
      def first_install_tasks
        @cloud.first_install_tasks_for(@instances) || []
      end
      def configure_tasks(a=[])
        @configure_tasks ||= a
      end
      # Custom installation tasks
      # Allow the remoter bases to attach their own tasks on the 
      # installation process
      def custom_install_tasks
        @cloud.custom_install_tasks_for(@instance) || []
      end
      # Custom configure tasks
      # Allows the remoter bases to attach their own
      # custom configuration tasks to the configuration process
      def custom_configure_tasks
        @cloud.custom_configure_tasks_for(@instance) || []
      end
      
      # Get the packages associated with each os
      def puppet_packages
        case @os
        when :fedora
          "puppet-server puppet factor"
        else
          "puppet puppetmaster"
        end
      end    
      # Package installers for general *nix operating systems
      def self.installers
        @installers ||= {
          :ubuntu => "aptitude install -y",
          :fedora => "yum install",
          :gentoo => "emerge"
        }
      end
      # Convenience method to grab the installer
      def installer_for(names=[])
        packages = names.is_a?(Array) ? names.join(" ") : names
        "#{self.class.installers[@os]} #{packages}"
      end
            
      def base_gems
        {
          :logging => "http://rubyforge.org/frs/download.php/44731/logging-0.9.4.gem",
          :ZenTest => "http://rubyforge.org/frs/download.php/45581/ZenTest-3.11.0.gem",
          :ParseTree => "http://rubyforge.org/frs/download.php/45600/ParseTree-3.0.1.gem",
          :ruby2ruby => "http://rubyforge.org/frs/download.php/45587/ruby2ruby-1.2.0.gem",
          :activesupport => "http://rubyforge.org/frs/download.php/45627/activesupport-2.1.2.gem",
          :"xml-simple" => "http://rubyforge.org/frs/download.php/18366/xml-simple-1.0.11.gem",
          :RubyInline => "http://rubyforge.org/frs/download.php/45683/RubyInline-3.8.1.gem",
          :flexmock => "http://rubyforge.org/frs/download.php/42580/flexmock-0.8.3.gem",
          :hoe => "http://rubyforge.org/frs/download.php/45685/hoe-1.8.2.gem",
          :lockfile => "http://rubyforge.org/frs/download.php/18698/lockfile-1.4.3.gem",
          :rubyforge => "http://rubyforge.org/frs/download.php/45546/rubyforge-1.0.1.gem",
          :rake => "http://rubyforge.org/frs/download.php/43954/rake-0.8.3.gem",
          :sexp_processor => "http://rubyforge.org/frs/download.php/45589/sexp_processor-3.0.0.gem",
          :capistrano => "http://rubyforge.org/frs/download.php/48031/capistrano-2.5.3.gem",
          :poolparty => "http://github.com/auser/poolparty/tree/master%2Fpkg%2Fpoolparty.gem?raw=true"
        }
      end
      
      # Template directory from the provisioner base
      def template_directory
        File.join(File.dirname(__FILE__), "..", "templates")
      end
                  
      # Install from the class-level
      def self.install(instance, cl=self)
        new(instance, cl).process_install!
      end

      def self.configure(instance, cl=self)
        new(instance, cl).process_configure!
      end
      
      def define_task(name, roles, &block)
        @config.task task_sym(name), :roles => roles, &block
      end

      def run(task)
        @config.send task_sym(task)
      end

      def task_sym(name)
        "install_#{name.to_task_name}".to_sym
      end
    end
  end  
end

## Load the provisioners
Dir[File.dirname(__FILE__) + "/provisioner_tasks/*.rb"].each do |file|
  require file
end
