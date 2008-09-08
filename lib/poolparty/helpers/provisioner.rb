# Provisioners
# 
# The provisioning for servers is handled by these provisioners

module Provisioner
  class ProvisionerBase
    def initialize
      build_task_list
    end
    def build_task_list
      tasks.each do |task|
        
      end
    end
    # This is the actual runner for the installation    
    def install      
    end
    # Build a list of the tasks to run on the instance
    def tasks(a=[])
      @tasks ||= a
    end
    # Package installers for general oss
    def self.installers
      @installers ||= {
        :ubuntu => "apt-get",
        :fedora => "yum"
      }
    end
    def self.installer_for(name)
      installers[name.to_sym]
    end
    def self.install
      new.install
    end    
  end
end

## Load the provisioners
Dir[File.dirname(__FILE__) + "/provisioners/*.rb"].each do |file|
  require file
end
