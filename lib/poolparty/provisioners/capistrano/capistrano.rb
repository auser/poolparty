require "poolparty/capistrano"

module PoolParty
  module Provisioner
    class Capistrano < ProvisionerBase
      
      include PoolParty::Capistrano
      include ::Capistrano::Configuration::Actions::Invocation
      
      def process_install!(testing=false)
        unless testing
          @cloud.rsync_storage_files_to(@instance)
          roles = provision_master? ? [:master] : [:slaves]
          run_capistrano(roles,:install)
        end
      end
      def process_configure!(testing=false)
        unless testing
          @cloud.rsync_storage_files_to(@instance)
          roles = provision_master? ? [:master] : [:slaves]
          run_capistrano(roles, :configure)
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
          "master:provision_master"
        ].push([custom_install_tasks, master_configure_tasks]).flatten#.map {|a| a.to_sym }
      end
      def master_configure_tasks
        [
          "master:configure_master_task"
        ].push(custom_configure_tasks).flatten#.map {|a| a.to_sym }
      end
      
      def slave_install_tasks
        [
          "slave:add_master_to_hosts_file", "slave:add_provisioner_configs", "setup_provisioner_config",
          "create_puppetrunner_command", "create_puppetrerun_command", "install_rubygems",
          "install_provisioner", "slave:stop_provisioner_daemon"
        ].push([custom_install_tasks, slave_configure_tasks]).flatten#.map {|a| a.to_sym }
      end
      def slave_configure_tasks
        [
          "run_provisioner"
        ].push(custom_configure_tasks).flatten#.map {|a| a.to_sym }
      end
      # Run tasks after the initialized
      def loaded
        create_config
      end
      
      # Create the config for capistrano
      # This is a dynamic capistrano configuration file
      def create_config        
        @config = ::Capistrano::Configuration.new
        if @cloud.debug || @cloud.verbose 
          @config.logger.level = @cloud.debug ? ::Capistrano::Logger::MAX_LEVEL : ::Capistrano::Logger::INFO
        else
          @config.logger.level = ::Capistrano::Logger::IMPORTANT
        end
        
        capfile = returning Array.new do |arr|
          Dir["#{::File.dirname(__FILE__)}/recipies/*.rb"].each {|a| arr << "require '#{a}'" }
          arr << "ssh_options[:keys] = '#{@cloud.full_keypair_basename_path}'"
          
          arr << set_poolparty_roles
        end.join("\n")
        
        @config.provisioner = self
        @config.cloud = @cloud
        
        @config.load(:string => capfile)
        
        @cloud.deploy_file ? @config.load(@cloud.deploy_file) : @config.set(:user, @cloud.user)
      end
      
      # Prerun
      def prerun_setup
      end
            
      def run_capistrano(roles=[:master], meth=:install)  
        prerun_setup
        
        commands = meth == :install ? install_tasks : configure_tasks
        name = "provisioner_#{meth}"
        
        __define_task(name, roles) do
          commands.map {|command| 
            task = find_task(command)
            task.options.merge!(:roles => roles)
            execute_task task
          }
        end
                
        begin
          __run(name)
          return true
        rescue ::Capistrano::CommandError => e
          return false unless verbose
          raise ProvisionerException.new("Error: #{e}")
        end
      end
                  
      def __define_task(name, roles, &block)
        @config.task __task_sym(name), :roles => roles, &block
      end

      def __run(task)
        @config.send __task_sym(task)
      end

      def __task_sym(name)
        "#{name.to_s.downcase.underscore}".to_sym
      end
      
    end    
  end
end