module Provisioner
  class Master < ProvisionerBase
    
    def valid?
      !(@cloud.nil? || @cloud.master.nil?)
    end
    
    def error
      raise RemoteException.new(:could_not_install, "Your cloud does not have a master")
    end
    
    def tasks
      [
        install_puppet_master,
        create_local_hosts_entry,
        setup_basic_structure,
        setup_configs,        
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
        if [ -z "grep -v '#' /etc/hosts | grep 'puppet'" ]; then echo '#{@master_ip}           puppet master' >> /etc/hosts; fi
      EOS
    end
    
    def setup_basic_structure
      <<-EOS
        puppetmasterd --mkusers
        mkdir -p #{Base.remote_storage_path}
        echo "import 'nodes/*.pp'" > /etc/puppet/manifests/site.pp
        echo "import 'classes/*.pp'" >> /etc/puppet/manifests/site.pp
        mkdir -p /etc/puppet/manifests/nodes 
        mkdir -p /etc/puppet/manifests/classes
      EOS
    end
    
    def setup_configs
      <<-EOS
        echo "#{open(File.join(template_directory, "puppet.conf")).read}" > /etc/puppet/puppet.conf
      EOS
    end
    
    def setup_fileserver
      <<-EOS
        echo "#{open(File.join(template_directory, "fileserver.conf")).read}" > /etc/puppet/fileserver.conf
        mkdir -p /var/poolparty/facts
        mkdir -p /var/poolparty/files
      EOS
    end
    # Change this eventually for better security supportsetup_fileserver
    def setup_autosigning
      <<-EOS
        echo "*" > /etc/puppet/autosign.conf
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
node "#{ri.ip}" {}
         EOS
       end
      "echo '#{str}' > /etc/puppet/manifests/nodes/nodes.pp"
    end
    
    def create_poolparty_manifest
      <<-EOS
        mv #{Base.remote_storage_path}/#{Base.tmp_path}/poolparty.pp /etc/puppet/manifests/classes
      EOS
    end
  end
end