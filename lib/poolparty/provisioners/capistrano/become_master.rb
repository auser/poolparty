module PoolParty
  module Provisioner
    class BecomeMaster < ProvisionerBase
      
      def initialize(cl=self, os=:ubuntu)
        raise MasterException.new(:no_ip) unless cl.master && cl.master.ip
        super(cl.master, cl, os)
        @master_ip = cl.master.ip
      end

      def valid?
        !(@cloud.nil? || @cloud.master.nil?)
      end

      def error
        raise RemoteException.new(:could_not_install, "Your cloud does not have a master")
      end
      
      def first_install_tasks
        [
          create_local_hosts_entry
        ]
      end

      def install_tasks
        [          
          setup_basic_structure,
          setup_configs,        
          setup_fileserver,
          setup_autosigning,
          restart_puppetmaster,
          run_first_time,
          create_local_node,
        ] << configure_tasks
      end

      def configure_tasks
        [
          create_local_node,
          move_templates,
          setup_poolparty,
          create_poolparty_manifest,
          restart_puppetd
        ]
      end
      
      # If the master is not in the hosts file, then add it to the hosts file
      def create_local_hosts_entry
        <<-EOS
echo "Creating local host entry"
if [ -z \"$(grep -v '#' /etc/hosts | grep 'puppet')" ]; then echo '#{@master_ip}          master puppet localhost' >> /etc/hosts; fi
hostname master
        EOS
      end

      def setup_basic_structure
        <<-EOS
echo "Creating basic structure for poolparty"        
mkdir -p /etc/puppet/manifests/nodes 
mkdir -p /etc/puppet/manifests/classes
echo "import 'nodes/*.pp'" > /etc/puppet/manifests/site.pp
echo "import 'classes/*.pp'" >> /etc/puppet/manifests/site.pp
cp #{Base.remote_storage_path}/namespaceauth.conf /etc/puppet/namespaceauth.conf
        EOS
      end

      def setup_configs
        <<-EOS
echo "Setting up configuration"        
cp #{Base.remote_storage_path}/puppet.conf /etc/puppet/puppet.conf
        EOS
      end

      def setup_fileserver
        <<-EOS
echo "Setting up the master fileserver"
echo "
[files]
  path #{Base.remote_storage_path}
  allow *" > /etc/puppet/fileserver.conf
mkdir -p /var/poolparty/facts
mkdir -p /var/poolparty/files
mkdir -p /etc/poolparty
        EOS
      end
      # Change this eventually for better security supportsetup_fileserver
      def setup_autosigning
        <<-EOS
echo "Creating accessibility for the nodes"        
echo "*" > /etc/puppet/autosign.conf
        EOS
      end
      
      def setup_poolparty
        <<-EOS
echo "Setting the poolparty configuration"
cp #{Base.remote_storage_path}/#{Base.key_file_locations.first} "#{Base.base_config_directory}/.ppkeys"
mv #{Base.remote_storage_path}/#{Base.default_specfile_name} #{Base.base_config_directory}/
        EOS
      end
            
      def copy_ssh_app
        "cp #{Base.remote_storage_path}/#{@cloud.full_keypair_name} #{@cloud.remote_keypair_path}" if @cloud.remote_keypair_path != "#{Base.remote_storage_path}/#{@cloud.full_keypair_name}"
      end
            
      # /etc/init.d/puppetmaster stop; rm -rf /etc/puppet/ssl; /etc/init.d/puppetmaster start
      # ps aux | grep puppetmaster | grep -v grep | awk '{print $2}' | xargs kill;
      def restart_puppetmaster
        <<-EOS
echo "(Re)starting poolparty"
. /etc/profile
/etc/init.d/puppetmaster stop;rm -rf /etc/poolparty/ssl;puppetmasterd --verbose;/etc/init.d/puppetmaster start
        EOS
      end
      
      def run_first_time
<<-EOE
echo "Running first time run"
cp #{Base.remote_storage_path}/#{Base.template_directory}/puppetrunner /usr/bin/puppetrunner
chmod +x /usr/bin/puppetrunner
EOE
      end

      # TODO:
      # Consider this method in the manifest
      def create_local_node
        str = <<-EOS
node default {
  include poolparty
}
        EOS
         @cloud.list_of_running_instances.each do |ri|
           str << <<-EOS           
node "#{ri.name}" inherits default {}
           EOS
         end
"echo '#{str}' > #{Base.manifest_path}/nodes/nodes.pp"
      end

      def move_templates
        <<-EOS
echo "Moving templates into place"
mkdir -p #{Base.template_path}
cp -R #{Base.remote_storage_path}/#{Base.template_directory}/* #{Base.template_path}
        EOS
      end
      
      def create_poolparty_manifest
        <<-EOS
echo "Creating the manifest"
cp #{Base.remote_storage_path}/poolparty.pp /etc/puppet/manifests/classes/poolparty.pp
#{copy_ssh_app}
        EOS
      end
            
      def restart_puppetd
        # /usr/bin/puppetrerun
        # /usr/bin/puppetcleaner master
        <<-EOS
echo "Running puppet manifest"
/usr/bin/puppetrerun
        EOS
      end
    end
  end
end