=begin rdoc
  The Provisioner is responsible for provisioning REMOTE servers
  This class only comes in to play when calling the setup commands on
  the development machine
=end
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
    
    class ProvisionerBase
      
      include Configurable
      include CloudResourcer
      
      def initialize(instance,cloud=self, os=:ubuntu)
        @instance = instance
        @cloud = cloud
        
        options(cloud.options) if cloud && cloud.respond_to?(:options)
        set_vars_from_options(instance.options) unless instance.options.empty?                
        options(instance.options) if instance.respond_to?(:options)
        
        @os = os.to_s.downcase.to_sym
        loaded
      end
      # Callback after initialized
      def loaded      
      end
      # This is the actual runner for the installation    
      def install
        valid? ? install_string : error
      end
      def write_install_file
        error unless valid?
        provisioner_file = ::File.join(Base.storage_directory, "install_#{name}.sh")
        ::File.open(provisioner_file, "w+") {|f| f << install }
      end
      def process_install!(testing=false)
        error unless valid?
        write_install_file
        setup_runner(@cloud)
        
        unless testing
          puts "Logging on to #{@instance.ip}"
          @cloud.rsync_storage_files_to(@instance)
          
          cmd = "cd #{Base.remote_storage_path}/#{Base.tmp_path} && chmod +x install_#{name}.sh && /bin/sh install_#{name}.sh && rm -rf *"
          hide_output do
            @cloud.run_command_on(cmd, @instance)
          end          
        end        
      end
      def configure
        valid? ? configure_string : error
      end
      def write_configure_file
        error unless valid?
        provisioner_file = ::File.join(Base.storage_directory, "configure_#{name}.sh")
        ::File.open(provisioner_file, "w+") {|f| f << configure }
      end
      def process_configure!(testing=false)
        error unless valid?
        setup_runner(@cloud)
        write_configure_file
        
        unless testing
          @cloud.rsync_storage_files_to(@instance)

          cmd = "cd #{Base.remote_storage_path}/#{Base.tmp_path} && chmod +x configure_#{name}.sh && /bin/sh configure_#{name}.sh && rm -rf *"
          @cloud.run_command_on(cmd, @instance)
        end
      end
      def setup_runner(cloud)
        cloud.prepare_to_configuration
        cloud.build_and_store_new_config_file
      end
      def valid?
        true
      end
      def error
        "Error in installation"
      end
      # Gather all the tasks into one string
      def install_string
        (default_install_tasks << custom_install_tasks).each do |task|
          case task.class
          when String
            task
          when Method
            self.send(task.to_sym)
          end
        end.nice_runnable
      end
      def configure_string
        (default_configure_tasks << custom_configure_tasks).each do |task|
          case task.class
          when String
            task
          when Method
            self.send(task.to_sym)
          end
        end.nice_runnable
      end
      # Tasks with default tasks 
      # These are run on all the provisioners, master or slave
      def default_install_tasks
        [
          
        ] << install_tasks
      end
      # Tasks with default configuration tasks
      # This is run on the provisioner, regardless
      def default_configure_tasks
        [
        ] << configure_tasks
      end
      # Build a list of the tasks to run on the instance
      def install_tasks(a=[])
        @install_task ||= a
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
      def get_puppet_packages_for(os)
        case os
        when :fedora
          "puppet-server puppet factor"
        else
          "puppet puppetmaster"
        end
      end    
      # Package installers for general *nix operating systems
      def self.installers
        @installers ||= {
          :ubuntu => "apt-get install -y",
          :fedora => "yum install",
          :gentoo => "emerge"
        }
      end
      # Convenience method to grab the installer
      def installer_for(name)
        self.class.installers[name.to_sym]
      end
      # Install from the class-level
      def self.install(instance, cl=self)
        new(instance, cl).install
      end

      def self.configure(instance, cl=self)
        new(instance, cl).configure
      end

      def template_directory
        File.join(File.dirname(__FILE__), "..", "templates")
      end

      def create_local_node
        str = <<-EOS
  node default {
    include poolparty
  }
        EOS
         @cloud.list_of_running_instances.each do |ri|
           str << <<-EOS           
  node "#{ri.name}" {}
           EOS
         end
        "echo '#{str}' > /etc/puppet/manifests/nodes/nodes.pp"
      end

      def create_poolparty_manifest
        <<-EOS
          mv #{Base.remote_storage_path}/poolparty.pp /etc/puppet/manifests/classes
        EOS
      end
    end
  end  
end
## Load the provisioners
Dir[File.dirname(__FILE__) + "/provisioners/*.rb"].each do |file|
  require file
end
