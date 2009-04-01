=begin rdoc
  The Provisioner is responsible for provisioning REMOTE servers
  This class only comes in to play when calling the setup commands on
  the development machine
=end
require "capistrano"
require 'capistrano/cli'

module PoolParty
  module Provisioner
    
    def provisioner_for(inst, caller=self)
      PoolParty::Provisioner::Capistrano.new(inst, caller, :ubuntu)
    end
    
    class ProvisionerBase
      attr_accessor :config, :loaded_tasks, :instance, :cloud, :os
      
      include Dslify
      include CloudResourcer
      include FileWriter
      
      def initialize(instance=nil, cld=nil, os=:ubuntu, &block)
        @instance = instance
        @cloud = cld
        options(cloud.options) if cloud && cloud.respond_to?(:options)
        
        dputs "Using key at: #{cld.keypair}"
        
        @os = os.to_s.downcase.to_sym
        self.instance_eval &block if block
        
        loaded
        
        # PoolPartyBootStrapper.new(options).execute!
      end
      
      # deprecate
      def provision_master? 
        !@instance.nil? && @instance.master?
      end
      
      def roles_to_provision
        [:master]  #always provision the master role for now.  When do we ever want anything else? MF
      end

      # Callback after initialized
      def loaded
      end
      
      def loaded_tasks
        @loaded_tasks ||= []
      end
      
      ### Installation tasks
      
      # This is the actual runner for the installation
      def install(testing=false)
        error unless valid?
        setup_runner
        unless testing
          before_install(@instance)

          vputs "Provisioning #{@instance}"
          process_install!(testing)

          after_install(@instance)
        end
      end
      # The provisioner bases overwrite this method
      def process_install!(testing=false)
        # raise ProvisionerException(" process_install! should be overwritten by provioner, but it was not.") #MF todo
      end
      
      # Configuration
      def configure(testing=false)
        error unless valid?
        setup_runner
        unless testing
          before_configure(@instance)

          vputs "Provisioning #{@instance.name}"
          process_configure!(testing)

          after_configure(@instance)
        end
      end
      
      # Tasks that need to be performed everytime we do any
      # remote ssh'ing into any instance
      def setup_runner(force=false)
        @cloud.prepare_for_configuration
        @cloud.build_and_store_new_config_file(force)
        Neighborhoods.clump(@cloud.remote_instances_list, "#{Default.tmp_path}/neighborhood.json") unless testing
      end
      
      # Callbacks
      # Before installation callback
      def before_install(instance)
      end
      def after_install(instance)
      end
      def before_configure(instance)
      end
      def after_configure(instance)
      end
      
      def valid?
        true
      end
      def error
        raise ProvisionerException.new("Error in installation")
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
      
      # Last install tasks, if need to install after everything else
      def after_install_tasks
        @cloud.after_install_tasks_for(@instance)
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
      def os_installer
        "#{self.class.installers[@os]}"
      end
      # Convenience method to grab the installer
      def installer_for(names=[])
        packages = names.is_a?(Array) ? names.join(" ") : names
        "#{self.class.installers[@os]} #{packages}"
      end
      
      #TODO#
      # Abstract the gems out
      def base_gems
        {
          :logging          => "http://rubyforge.org/frs/download.php/44731/logging-0.9.4.gem",
          :ZenTest          => "http://rubyforge.org/frs/download.php/45581/ZenTest-3.11.0.gem",
          :ParseTree        => "http://rubyforge.org/frs/download.php/45600/ParseTree-3.0.1.gem",
          :ruby2ruby        => "http://rubyforge.org/frs/download.php/45587/ruby2ruby-1.2.0.gem",
          :activesupport    => "http://rubyforge.org/frs/download.php/45627/activesupport-2.1.2.gem",
          :"xml-simple"     => "http://rubyforge.org/frs/download.php/18366/xml-simple-1.0.11.gem",
          :RubyInline       => "http://rubyforge.org/frs/download.php/45683/RubyInline-3.8.1.gem",
          :flexmock         => "http://rubyforge.org/frs/download.php/42580/flexmock-0.8.3.gem",
          :lockfile         => "http://rubyforge.org/frs/download.php/18698/lockfile-1.4.3.gem",
          :rake             => "http://rubyforge.org/frs/download.php/43954/rake-0.8.3.gem",
          :sexp_processor   => "http://rubyforge.org/frs/download.php/45589/sexp_processor-3.0.0.gem",
          "net-ssh"         => "http://rubyforge.org/frs/download.php/51288/net-ssh-2.0.10.gem",
          "net-sftp"        => "http://rubyforge.org/frs/download.php/37669/net-sftp-2.0.1.gem",
          "net-scp"         => "http://rubyforge.org/frs/download.php/37664/net-scp-1.0.1.gem",
          "net-ssh-gateway" => "http://rubyforge.org/frs/download.php/36389/net-ssh-gateway-1.0.0.gem",
          :echoe            => "http://rubyforge.org/frs/download.php/51240/echoe-3.1.gem",
          :highline         => "http://rubyforge.org/frs/download.php/46328/highline-1.5.0.gem",
          :capistrano       => "http://rubyforge.org/frs/download.php/51294/capistrano-2.5.4.gem",
          :poolparty        => "http://github.com/auser/poolparty/tree/master%2Fpkg%2Fpoolparty.gem?raw=true",
          "ec2"             => "http://rubyforge.org/frs/download.php/43666/amazon-ec2-0.3.1.gem"
        }
      end
      
      def download_base_gems_string
        returning(Array.new) do |arr|
          base_gems.each do |name, url|
            arr << "wget #{url} -O #{Default.remote_storage_path}/#{name}.gem 2>&1"
          end
        end.join(" && ")
      end
      
      def install_base_gems_string
        returning(Array.new) do |arr|
          base_gems.each do |name, url|
            arr << "/usr/bin/gem install --ignore-dependencies --no-ri --no-rdoc #{Default.remote_storage_path}/#{name}.gem"
          end
        end.join(" && ")
      end
      
      # Template directory from the provisioner base
      def template_directory
        File.join(File.dirname(__FILE__), "..", "templates")
      end
      
      # Install from the class-level
      def self.install(instance, cl=self, testing=false)
        new(instance, cl).install(testing)
      end

      def self.configure(instance, cl=self, testing=false)
        new(instance, cl).configure(testing)
      end
      
    end
  end
end

## Load the provisioners
Dir[File.dirname(__FILE__) + "/*/*.rb"].each do |file|
  require file
end
