require "poolparty/capistrano"

module PoolParty
  module Provisioner
    class Capistrano < ProvisionerBase
      
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
          "custom_install_tasks",
          "master_provision_master_task",
          "after_install_tasks",
          "custom_configure_tasks",
          "run_provisioner_twice",
          "master_configure_master_task"
        ]#.map {|a| a.to_sym }
      end
      def master_configure_tasks
        [
          "master_configure_master_task"
        ]#.map {|a| a.to_sym }
      end
      
      def slave_install_tasks
        [
          "custom_install_tasks",
          "slave_provision_slave_task",
          "after_install_tasks",
          "custom_configure_tasks",
          "slave_configure_slave_task"
        ]
      end
      def slave_configure_tasks
        [
          "custom_configure_tasks",
          "slave_configure_slave_task"
        ]#.flatten.map {|a| a.to_sym }
      end
      # Run tasks after the initialized
      def loaded
        create_config
      end
      
      def set_poolparty_roles
        returning Array.new do |arr|
          arr << "role 'master.#{@cloud.name}'.to_sym, '#{@cloud.master.ip}'"
          arr << "role :master, '#{@cloud.master.ip}'"
          arr << "role :slaves, '#{@cloud.nonmaster_nonterminated_instances.map{|a| a.ip}.join('", "')}'" if @cloud.nonmaster_nonterminated_instances.size > 0
          arr << "role :single, '#{@instance.ip}'" if @instance && @instance.ip
        end.join("\n")
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
      
      # In run_capistrano, we are going to run the entire capistrano process
      # First, 
      def run_capistrano(roles=[:master], meth=:install)  
        prerun_setup
        
        commands = meth == :install ? install_tasks : configure_tasks
        name = "#{roles.first}_provisioner_#{meth}"

        __define_task(name, roles) do
          commands.map {|command|
            task = find_task(command.to_sym)            
            
            if task
              task.options.merge!(:roles => roles)
              execute_task task
            else
              if provisioner.respond_to?(command.to_sym)
                cmd = provisioner.send(command.to_sym)
                cmd = cmd.join(" && ") if cmd.is_a?(Array)
                run(cmd)
              else
                self.send(command.to_sym)
              end
            end
          }
        end
                
        begin
          __run(name)
          return true
        rescue ::Capistrano::CommandError => e
          return false unless verbose
          puts "Error: #{e} " and raise ProvisionerException.new("Error: #{e}")
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