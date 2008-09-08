module Provisioner
  class Slave < ProvisionerBase
    attr_reader :master_ip
    def initialize(ip="127.0.0.2", master_ip="127.0.0.1", os=:ubuntu)
      @ip = ip
      @master_ip = master_ip
      @os = os
    end
    def tasks
      [
        install_puppet
      ]
    end
    
    def install_puppet
      "#{installer_for(@os)} #{get_puppet_packages_for(@os)}"
    end
  end
end