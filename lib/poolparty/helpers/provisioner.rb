# Provisioners
# 
# The provisioning for servers is handled by these provisioners

module Provisioner
  class ProvisionerBase
    # Stub method for child classes to overwrite
    def self.install      
    end
    
    def self.installers
      @installers ||= {
        :ubuntu => "apt-get",
        :fedora => "yum"
      }
    end
    
    def self.installer(name)
      installers[name.to_sym]
    end
    
  end
end

## Load the provisioners
Dir[File.dirname(__FILE__) + "/provisioners/*.rb"].each do |file|
  require file
end
