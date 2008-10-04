module Provisioner
  class Slave < ProvisionerBase
    attr_reader :master_ip
    
    def initialize(cloud, os=:ubuntu)
      @cloud = cloud
      @os = os
      
      @master_ip = cloud.master.ip
    end
    def tasks
      [
        install_puppet,
        setup_configs,
        setup_puppet
      ]
    end
    
    def install_puppet
      <<-EOE        
        #{installer_for(@os)} #{get_puppet_packages_for(@os)}
        echo 'DAEMON_OPTS="-w 120 –server #{@master_ip}"' > /etc/default/puppet
      EOE
    end
    
    def setup_puppet
      <<-EOE
        puppetd --mkusers
        if [ -z "grep -v '#' /etc/hosts | grep 'puppet'" ]; then echo '#{@master_ip}           puppet master' >> /etc/hosts; fi
        puppetd --listen
      EOE
    end
    
    def setup_configs
      <<-EOS
        echo "#{open(File.join(template_directory, "puppet.conf")).read}" > /etc/puppet/puppet.conf
        mv #{Base.remote_storage_path}/#{Base.tmp_path}/namespaceauth.conf /etc/puppet/namespaceauth.conf
      EOS
    end
    
  end
end