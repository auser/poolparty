module Provisioner
  class Master < ProvisionerBase
    
    def tasks
      [
        install_puppet_master,
        create_local_hosts_entry,
        setup_configs,
        setup_basic_structure,
        create_basic_site_pp,
        setup_fileserver,
        setup_autosigning,
        create_local_node,
        create_poolparty_manifest
      ]
    end
    
    def install_puppet_master
      "#{installer_for(@os)} #{get_puppet_packages_for(@os)}"
    end
        
    def create_local_hosts_entry
      <<-EOS
        echo "#{@cloud.master.ip}             puppet master" >> /etc/hosts
      EOS
    end
    
    def setup_basic_structure
      <<-EOS
        puppetmasterd --mkusers        
        echo "import 'nodes/*.pp'" > /etc/puppet/manifests/site.pp
        echo "import 'classes/*.pp'" >> /etc/puppet/manifests/site.pp
        mkdir /etc/puppet/manifests/nodes /etc/puppet/manifests/classes
      EOS
    end
    
    def setup_configs
      <<-EOS
        echo "#{open(File.join(template_directory, "puppet.conf")).read}" > /etc/puppet/puppet.conf
      EOS
    end
    
    def create_basic_site_pp
      <<-EOS
        echo "node default {
            include poolparty
        }" >> /etc/puppet/manifests/site.pp
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
      str = <<-EOS
        node default {
          include poolparty
        }
      EOS
       @cloud.list_from_remote(:do_not_cache => true).each do |ri|
         str << <<-EOS           
           node "#{ri.name}" {}
         EOS
       end
      "echo #{str} > /etc/puppet/manifests/nodes/nodes.pp"
    end
    
    def create_poolparty_manifest
      
    end
  end
end