=begin rdoc
  The Provisioner is responsible for provisioning REMOTE servers
  This class only comes in to play when calling the setup commands on
  the development machine
=end
module Provisioner
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
      install_string
    end
    # Gather all the tasks into one string
    def install_string
      tasks.each do |task|
        case task.class
        when String
          task
        when Method
          self.send(task.to_sym)
        end
      end.nice_runnable
    end
    # Build a list of the tasks to run on the instance
    def tasks(a=[])
      @tasks ||= a
    end
    # Get the packages associated with each os
    def get_puppet_packages_for(os)
      case os
      when :fedora
        "puppet-server puppet factor"
      else
        "puppet factor"
      end
    end    
    # Package installers for general *nix operating systems
    def self.installers
      @installers ||= {
        :ubuntu => "apt-get install",
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
  end
end

## Load the provisioners
Dir[File.dirname(__FILE__) + "/provisioners/*.rb"].each do |file|
  require file
end
