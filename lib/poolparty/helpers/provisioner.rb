# Provisioners
# 
# The provisioning for servers is handled by these provisioners

module Provisioner
  class ProvisionerBase
    def initialize(ip="127.0.0.1", os=:ubuntu)
      @ip = ip
      @os = os.to_sym
    end
    # This is the actual runner for the installation    
    def install
      install_string
    end
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
    def self.install
      new.install
    end    
  end
end

## Load the provisioners
Dir[File.dirname(__FILE__) + "/provisioners/*.rb"].each do |file|
  require file
end
