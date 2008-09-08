module Provisioner
  class Master < ProvisionerBase
    
    def tasks
      [
        install_puppet_master,
        create_local_hosts_entry,
        create_basic_site_pp,
        setup_fileserver,
        setup_autosigning,
        create_local_node
      ]
    end
    
    def install_puppet_master
      "#{installer_for(@os)} #{get_puppet_packages_for(@os)}"
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
        mkdir /etc/puppet/manifests/nodes /etc/puppet/manifests/classes
      EOS
    end
    
    def setup_fileserver
      <<-EOS
        echo "[files]
          path /data/puppet/fileserver
          allow #{@ip}" > /etc/puppet/fileserver.conf
        mkdir -p /data/puppet/fileserver
      EOS
    end
    
    def setup_autosigning
      <<-EOS
        echo "*.#{@ip}" > /etc/puppet/autosign.conf
      EOS
    end
    
    def create_local_node
      <<-EOS
        node "master.#{@ip}" {
           include hosts
        }
      EOS
    end
  end
end