=begin rdoc
  The Provisioner is responsible for provisioning REMOTE servers
  This class only comes in to play when calling the setup commands on
  the development machine
=end
module Provisioner
  
  def self.provision_master(cloud, testing=false)
    with_cloud(cloud) do
      puts "Building install file"
      provisioner_file = ::File.join(Base.storage_directory, "install_master.sh")
      ::File.open(provisioner_file, "w+") do |file|
        file << Master.install(self)
      end
      
      puts "Syncing local configuration files with master"
      rsync_storage_files_to(master) unless testing
      
      puts "Logging on to the master and executing provisioning"
      cmd = "cd #{Base.remote_storage_path}/#{Base.tmp_path} && chmod +x install_master.sh && /bin/sh install_master.sh && rm install_master.sh && puppetmasterd"
      run_command_on(cmd, master) unless testing
    end
  end
  
  def self.configure_master(cloud, testing=false)
    with_cloud(cloud) do
      puts "Building install file"
      provisioner_file = ::File.join(Base.storage_directory, "configure_master.sh")
      ::File.open(provisioner_file, "w+") do |file|
        file << Master.configure(self)
      end
      
      puts "Syncing local configuration files with master"
      rsync_storage_files_to(master) unless testing
      
      puts "Logging on to the master and executing provisioning"
      cmd = "cd #{Base.remote_storage_path}/#{Base.tmp_path} && chmod +x configure_master.sh && /bin/sh configure_master.sh && rm configure_master.sh && puppetmasterd"
      run_command_on(cmd, master) unless testing
    end
  end
  
  def self.provision_slaves(cloud, testing=false)
    slaves = cloud.list_of_running_instances.reject {|a| a.master? }
    puts "Building slave install files"
    provisioner_file = ::File.join(Base.storage_directory, "install_slave.sh")
    ::File.open(provisioner_file, "w+") do |file|
      file << Slave.install(cloud)
    end
    
    slaves.each do |instance|
      puts "Syncing local configuration files with master"
      cloud.rsync_storage_files_to(instance) unless testing

      puts "Logging on to the slave (@ #{instance.ip}) and executing provisioning"
      cmd = "cd #{Base.remote_storage_path}/#{Base.tmp_path} && chmod +x install_slave.sh && /bin/sh install_slave.sh && rm install_slave.sh && puppetd"
      cloud.run_command_on(cmd, instance) unless testing
    end
  end
  
  def self.configure_slaves(cloud, testing=false)
    with_cloud(cloud) do
      puts "Building install file"
      provisioner_file = ::File.join(Base.storage_directory, "configure_slave.sh")
      ::File.open(provisioner_file, "w+") do |file|
        file << Slave.configure(self)
      end
      
      slaves = cloud.list_of_running_instances.reject {|a| a.master? }
      slaves.each do |instance|
        puts "Syncing local configuration files with slave"
        cloud.rsync_storage_files_to(instance) unless testing

        puts "Logging on to the slave (@ #{instance.ip}) and executing provisioning"
        cmd = "cd #{Base.remote_storage_path}/#{Base.tmp_path} && chmod +x configure_slave.sh && /bin/sh configure_slave.sh && rm configure_slave.sh && puppetd"
        cloud.run_command_on(cmd, instance) unless testing
      end
    end
  end
  
  class ProvisionerBase
    
    def initialize(cloud, os=:ubuntu)
      @cloud = cloud
      @os = os.to_s.downcase.to_sym
      set_ip
    end
    def set_ip      
      @ip = @cloud.master.ip if @cloud && @cloud.master && !@ip
    end
    # This is the actual runner for the installation    
    def install
      set_ip unless @ip      
      valid? ? install_string : error
    end
    def configure
      set_ip unless @ip
      valid? ? configure_string : error
    end
    def valid?
      true
    end
    def error
      "Error in installation"
    end
    # Gather all the tasks into one string
    def install_string
      install_tasks.each do |task|
        case task.class
        when String
          task
        when Method
          self.send(task.to_sym)
        end
      end.nice_runnable
    end
    def configure_string
      configure_tasks.each do |task|
        case task.class
        when String
          task
        when Method
          self.send(task.to_sym)
        end
      end.nice_runnable
    end
    # Tasks with default tasks 
    def default_tasks
      install_tasks
    end
    # Build a list of the tasks to run on the instance
    def install_tasks(a=[])
      @install_task ||= a
    end
    def configure_tasks(a=[])
      @configure_tasks ||= a
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
    def self.install(cl)
      new(cl).install
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
       @cloud.list_from_remote(:do_not_cache => true).each do |ri|
         str << <<-EOS           
node "#{ri.ip}" {}
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

## Load the provisioners
Dir[File.dirname(__FILE__) + "/provisioners/*.rb"].each do |file|
  require file
end
