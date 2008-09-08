module Provisioner
  class Master < ProvisionerBase
    
    def tasks
      [
        install_puppet_master,
        create_local_hosts_entry,
        create_basic_site_pp
      ]
    end
    
    def install_puppet_master
      "#{installer_for(@os)} install puppet factor"
    end
    
    def create_local_hosts_entry
      <<-EOS
        echo "#{@ip}             puppet" >> /etc/hosts
      EOS
    end
    
    def create_basic_site_pp
      <<-EOS
        echo "import 'nodes/*.pp'" > /etc/puppet/manifests/site.pp
        echo "import 'classes/*.pp'" >> /etc/puppet/manifests/site.pp
      EOS
    end
    
  end
end