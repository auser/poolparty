module PoolParty
  module Provisioner
    class Capistrano < ProvisionerBase
      
      def process_install!(testing=false);run_cap(:install) unless testing;end
      def process_configure!(testing=false);run_cap(:configure) unless testing;end
      
      def install_tasks
        provision_master? ? master_install_tasks : slave_install_tasks
      end
      def configure_tasks
        provision_master? ? master_configure_tasks : slave_configure_tasks
      end
      
      def master_install_tasks
        [
          "master:create_local_hosts_entry", "master:setup_basic_poolparty_structure",
          "master:setup_provisioner_filestore", "master:setup_provisioner_autosigning", "base:install_rubygems",
          "base:add_provisioner_configs","base:setup_provisioner_config",
          "base:install_provisioner", "base:create_puppetrunner_command", "base:create_puppetrerun_command",
          "master:download_base_gems","master:install_base_gems", "master:restart_provisioner_base"
        ].push(custom_install_tasks).map {|a| a.to_sym }
      end
      def master_configure_tasks
        [
          "master:create_local_node_entry_for_puppet", "master:move_template_files", "master:setup_poolparty_base_structure",
          "master:move_provisioner_manifest", "base:run_provisioner"
        ].push(custom_configure_tasks).map {|a| a.to_sym }
      end
      
      def slave_install_tasks
        [
          "slave:add_master_to_hosts_file", "slavbase:add_provisioner_configs", "base:setup_provisioner_config",
          "base:create_puppetrunner_command", "base:create_puppetrerun_command", "base:install_rubygems",
          "base:install_provisioner", "slave:stop_provisioner_daemon"
        ].push(custom_install_tasks).map {|a| a.to_sym }
      end
      def slave_configure_tasks
        [
          "base:run_provisioner"
        ].push(custom_configure_tasks).map {|a| a.to_sym }
      end
      # Run tasks after the initialized
      def loaded
        create_config
        create_roles
        setup_roles
      end
      
      # Create the roles for capistrano
      def create_roles
        @config.set :master, @cloud.master
        @config.set :slaves, @cloud.nonmaster_nonterminated_instances.join(", ")
        @config.set :all, @cloud.list_of_running_instances.join(", ")
      end
      
      # Create the config of Cap
      def create_config
        @config = ::Capistrano::Configuration.new
        @config.logger.level = @cloud.verbose ? ::Capistrano::Logger::INFO : ::Capistrano::Logger::IMPORTANT
        
        Dir["#{::File.dirname(__FILE__)}/*.rake"].each {|f| @config.load(f) }
        
        if @cloud.deploy_file
          @config.load @cloud.deploy_file 
        else
          set :user, @cloud.user
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
          raise ProvisionerException.new("#{e}")
        end
      end
      
      def setup_roles(all=false)
        @roles ||= all ? roles(:all) : (provision_master? ? roles(:master) : roles(:slaves))
      end
      
      # Set the roles for capistrano
      def roles(role=:master)
        @roles ||= [role]
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