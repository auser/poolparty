module PoolParty
  module Provisioner
    class Master < ProvisionerBase
      
      def initialize(cloud=self, os=:ubuntu)
        super(cloud.master, cloud)
      end

      def valid?
        !(@cloud.nil? || @cloud.master.nil?)
      end

      def error
        raise RemoteException.new(:could_not_install, "Your cloud does not have a master")
      end

      def install_tasks
        [
          install_puppet_master,
          create_local_hosts_entry,
          setup_basic_structure,
          setup_configs,        
          setup_fileserver,
          setup_autosigning,
          start_puppetmaster
        ] << configure_tasks
      end

      def configure_tasks
        [
          create_local_node,
          move_templates,
          create_poolparty_manifest,
          restart_puppetd
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
mv #{Base.remote_storage_path}/#{Base.tmp_path}/namespaceauth.conf /etc/puppet/namespaceauth.conf
        EOS
      end

      def setup_configs
        <<-EOS
echo "#{open(File.join(template_directory, "puppet.conf")).read}" > /etc/puppet/puppet.conf
        EOS
      end

      def setup_fileserver
        <<-EOS
echo "
[files]
  path #{Base.remote_storage_path}/#{Base.tmp_path}
  allow *" > /etc/puppet/fileserver.conf
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

      def move_templates
        <<-EOS
mkdir -p #{Base.template_path}
mv #{Base.remote_storage_path}/#{Base.template_directory}/* #{Base.template_path}
        EOS
      end

      def create_poolparty_manifest
        <<-EOS
mv #{Base.remote_storage_path}/#{Base.tmp_path}/poolparty.pp /etc/puppet/manifests/classes
        EOS
      end

      def start_puppetmaster
"puppetmasterd && puppetd --listen"
      end    

      def restart_puppetd # && puppetrun --host 127.0.0.1
        # "puppetd --listen"
      end
    end
  end
end