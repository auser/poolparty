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
          "master:provision_master"
        ].push([custom_install_tasks, master_configure_tasks]).flatten#.map {|a| a.to_sym }
      end
      def master_configure_tasks
        [
          "master:create_local_node_entry_for_puppet", "master:move_template_files", "master:setup_poolparty_base_structure",
          "master:move_provisioner_manifest", "run_provisioner"
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
            
      # Create the roles for capistrano
      # role :#{@cloud.name}master, "#{@cloud.name}-master"
      def role_string
        # @config.role "master.#{@cloud.name}".to_sym, "#{@cloud.master.ip}"
        # @config.role :slaves, "#{@cloud.nonmaster_nonterminated_instances.map{|a| a.ip}.join('", "')}"
        # @config.role :all, "#{@cloud.list_of_running_instances.map{|a| a.ip}.join('", "')}"
        <<-EOR          
          role :master, "#{@cloud.master.ip}"
          # role :slaves, '#{@cloud.nonmaster_nonterminated_instances.map{|a| a.ip}.join('", "')}'
          # role :all, '#{@cloud.list_of_running_instances.map{|a| a.ip }.join('", "')}'
        EOR
      end
      
      # Very ugly
      # This is a way to allow common variables to be accessed for the Capistrano
      # tasks. This should be abstracted
      def common_variables_string
        <<-EOS
          set :master_ip, '#{@cloud.master.ip}'
          set :puppet_packages, '#{puppet_packages}'
          set :installer_for, '#{os_installer}'
          set :os, '#{@os}'
          set :remote_storage_path, '#{Base.remote_storage_path}'
          set :poolparty_config_directory, '#{Base.poolparty_config_directory}'
          set :template_path, '#{Base.template_path}'
          set :template_directory, '#{Base.template_directory}'
          set :base_config_directory, '#{Base.base_config_directory}'
          set :install_base_gems_string, '#{install_base_gems_string}'
          set :download_base_gems_string, '#{download_base_gems_string}'
          set :manifest_path, '#{Base.manifest_path}'
          set :key_file_location, '#{Base.key_file_locations.first}'
          set :default_specfile_name, '#{Base.default_specfile_name}'
        EOS
      end
      
      # Create the config for capistrano
      # This is a dynamic capistrano configuration file
      def create_config
        @config = CapistranoConfigurer.new

        if @cloud.debug || @cloud.verbose 
          @config.logger.level = @cloud.debug ? ::Capistrano::Logger::MAX_LEVEL : ::Capistrano::Logger::INFO
        else
          @config.logger.level = ::Capistrano::Logger::IMPORTANT
        end
        
        @cloud.deploy_file ? @config.load(@cloud.deploy_file) : @config.set(:user, @cloud.user)
      end
      
      # Prerun
      def prerun_setup
        capfile = returning Array.new do |arr|
          Dir["#{::File.dirname(__FILE__)}/recipies/*.rb"].each {|a| arr << "require '#{a}'" }
          arr << "ssh_options[:keys] = '#{@cloud.full_keypair_basename_path}'"
          
          arr << role_string
          arr << common_variables_string
        end.join("\n")
        
        @config.load(:string => capfile)
      end
            
      def run_capistrano(roles=[:master], meth=:install)  
        prerun_setup
        
        # puts @config.invoke_command(@config.task_list(:all).last.fully_qualified_name, {:roles => [:master]})
        commands = meth == :install ? install_tasks : configure_tasks
        name = "provisioner_#{meth}"
        
        define_task(name, roles) do
          commands.each {|command| 
            find_and_execute_task(command.to_sym, :before => :start, :after => :finish) 
          }
        end
        
        # @config.task_list(:all).each {|t| puts "t: #{t.fully_qualified_name}"}  
                
        begin
          run(name)
          return true
        rescue ::Capistrano::CommandError => e
          return false unless verbose
          raise ProvisionerException.new("Error: #{e}")
        end
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