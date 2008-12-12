module PoolParty
  module Provisioner
    class Capistrano < ProvisionerBase
      
      include ::Capistrano::Configuration::Actions::Invocation
      
      def process_install!(testing=false)
        unless testing
          @cloud.rsync_storage_files_to(@instance)
          run_cap(:install)
        end
      end
      def process_configure!(testing=false)
        unless testing
          @cloud.rsync_storage_files_to(@instance)
          run_cap(:configure)
        end
      end
      
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
        ].push(custom_install_tasks).flatten#.map {|a| a.to_sym }
      end
      def master_configure_tasks
        [
          "master:create_local_node_entry_for_puppet", "master:move_template_files", "master:setup_poolparty_base_structure",
          "master:move_provisioner_manifest", "base:run_provisioner"
        ].push(custom_configure_tasks).flatten#.map {|a| a.to_sym }
      end
      
      def slave_install_tasks
        [
          "slave:add_master_to_hosts_file", "slavbase:add_provisioner_configs", "base:setup_provisioner_config",
          "base:create_puppetrunner_command", "base:create_puppetrerun_command", "base:install_rubygems",
          "base:install_provisioner", "slave:stop_provisioner_daemon"
        ].push(custom_install_tasks).flatten#.map {|a| a.to_sym }
      end
      def slave_configure_tasks
        [
          "base:run_provisioner"
        ].push(custom_configure_tasks).flatten#.map {|a| a.to_sym }
      end
      # Run tasks after the initialized
      def loaded
        create_config
      end
            
      # Create the roles for capistrano
      def create_roles
        @config.role :master, "#{@cloud.master.ip}"
        @config.role :slaves, @cloud.nonmaster_nonterminated_instances.map{|a| a.ip}.join(", ")
        @config.role :all, @cloud.list_of_running_instances.map{|a| a.ip}.join(", ")
      end
      
      # Create the config for capistrano
      def create_config        
        @config = ::Capistrano::Configuration.new
        @config.logger.level = @cloud.verbose ? ::Capistrano::Logger::INFO : ::Capistrano::Logger::IMPORTANT        
        
        Dir["#{::File.dirname(__FILE__)}/tasks/*.rb"].each {|a| @config.load a }                      
        
        @config.ssh_options[:keys] = [@cloud.full_keypair_basename_path]
        
        if @cloud.deploy_file
          @config.load @cloud.deploy_file 
        else
          @config.set :user, @cloud.user
        end
      end
      
      # Prerun
      def prerun_setup
        create_roles
        setup_roles
      end
            
      def run_cap(meth=:install)  
        prerun_setup
        
        # puts @config.invoke_command(@config.task_list(:all).last.fully_qualified_name, {:roles => [:master]})
        commands = meth == :install ? install_tasks : configure_tasks
        name = "provisioner_#{meth}"
        
        define_task(name, roles) do
          via = fetch(:run_method, :sudo)
          commands.each do |command|
            puts "Command: #{command}"
            find_and_execute_task(command.to_sym, :before => :start, :after => :finish)
          end
        end
        
        # @config.task_list(:all).each {|t| puts "t: #{t.fully_qualified_name}"}  
                
        begin
          run(name)
          # ::Capistrano::CLI.execute
          # @config.execute
          return true
        rescue ::Capistrano::CommandError => e
          return false unless verbose
          raise ProvisionerException.new("Error: #{e}")
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
        "#{name.to_s.downcase.underscore}".to_sym
      end
      
    end    
  end
end