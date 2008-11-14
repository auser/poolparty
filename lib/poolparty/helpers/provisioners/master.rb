module PoolParty
  module Provisioner
    class Master < ProvisionerBase
      
      def initialize(cloud=self, os=:ubuntu)
        raise MasterException.new(:no_ip) unless cloud.master && cloud.master.ip
        super(cloud.master, cloud, os)
        @master_ip = cloud.master.ip
      end

      def valid?
        !(@cloud.nil? || @cloud.master.nil?)
      end

      def error
        raise RemoteException.new(:could_not_install, "Your cloud does not have a master")
      end

      def install_tasks
        [
          create_local_hosts_entry,
          setup_basic_structure,
          setup_configs,        
          setup_fileserver,
          setup_autosigning,
          install_poolparty,
          setup_poolparty,
          create_local_node,
          restart_puppetmaster,
          run_first_time          
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
      
      # If the master is not in the hosts file, then add it to the hosts file
      def create_local_hosts_entry
        <<-EOS
echo "Creating local host entry"
if [ -z \"$(grep -v '#' /etc/hosts | grep 'puppet')" ]; then echo '#{@master_ip}           puppet master localhost' >> /etc/hosts; fi
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
echo "#{open(File.join(template_directory, "puppet.conf")).read}" > /etc/puppet/puppet.conf
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
cp #{Base.remote_storage_path}/#{Base.default_specfile_name} #{Base.base_config_directory}/#{Base.default_specfile_name}
        EOS
      end
            
      def copy_ssh_app
        "cp #{Base.remote_storage_path}/#{@cloud.full_keypair_name} #{@cloud.remote_keypair_path}" if @cloud.remote_keypair_path != "#{Base.remote_storage_path}/#{@cloud.full_keypair_name}"
      end
      
      def install_poolparty
        <<-EOE
echo "Installing poolparty"
cd /var/poolparty
wget http://rubyforge.org/frs/download.php/44731/logging-0.9.4.gem -O logging.gem 2>&1
wget http://rubyforge.org/frs/download.php/45581/ZenTest-3.11.0.gem -O ZenTest.gem 2>&1
wget http://rubyforge.org/frs/download.php/45600/ParseTree-3.0.1.gem -O ParseTree.gem 2>&1
wget http://rubyforge.org/frs/download.php/45587/ruby2ruby-1.2.0.gem -O ruby2ruby.gem 2>&1
wget http://rubyforge.org/frs/download.php/45627/activesupport-2.1.2.gem -O activesupport.gem 2>&1
wget http://rubyforge.org/frs/download.php/18366/xml-simple-1.0.11.gem -O xml-simple.gem 2>&1
wget http://rubyforge.org/frs/download.php/45683/RubyInline-3.8.1.gem -O RubyInline.gem 2>&1
wget http://rubyforge.org/frs/download.php/42580/flexmock-0.8.3.gem -O flexmock.gem 2>&1
wget http://rubyforge.org/frs/download.php/45685/hoe-1.8.2.gem -O hoe.gem 2>&1
wget http://rubyforge.org/frs/download.php/18698/lockfile-1.4.3.gem -O lockfile.gem 2>&1
wget http://rubyforge.org/frs/download.php/45546/rubyforge-1.0.1.gem -O rubyforge.gem 2>&1
wget http://rubyforge.org/frs/download.php/43954/rake-0.8.3.gem -O rake.gem 2>&1
wget http://rubyforge.org/frs/download.php/45589/sexp_processor-3.0.0.gem -O sexp_processor.gem 2>&1
wget http://github.com/auser/poolparty/tree/master%2Fpkg%2Fpoolparty.gem?raw=true -O poolparty.gem 2>&1
wget http://rubyforge.org/frs/download.php/43666/amazon-ec2-0.3.1.gem -O amazon-ec2.gem 2>&1

#{
  %w(rake lockfile rubyforge hoe ZenTest sexp_processor flexmock logging activesupport 
      RubyInline ParseTree ruby2ruby xml-simple poolparty amazon-ec2).map do |dep|
    "gem install --ignore-dependencies -y --no-ri --no-rdoc #{dep}.gem #{unix_hide_string}"
  end.join("\n")
}
        EOE
      end
      
      # ps aux | grep puppetmasterd | awk '{print $2}' | xargs kill
      # /etc/init.d/puppetmaster stop; rm -rf /etc/puppet/ssl; /etc/init.d/puppetmaster start
      def restart_puppetmaster
        <<-EOS
echo "(Re)starting poolparty"
# . /etc/profile
# /etc/init.d/puppetmaster stop #{unix_hide_string}
# ps aux | grep puppetmaster | awk '{print $2}' | xargs kill #{unix_hide_string} # just in case
# rm -rf /etc/puppet/ssl
# # Start it back up
# # puppetmasterd --verbose
/etc/init.d/puppetmaster start
        EOS
      end
      
      def run_first_time
<<-EOE
mv #{Base.remote_storage_path}/#{Base.template_directory}/puppetrerun /usr/bin/puppetrerun
chmod +x /usr/bin/puppetrerun
/bin/sh /usr/bin/puppetrerun
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
"echo '#{str}' > /etc/puppet/manifests/nodes/nodes.pp"
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
      
      def clean_master_certs
        str = returning Array.new do |s|
          s << "puppetca --clean master.compute-1.internal 2>&1 > /dev/null"
          s << "puppetca --clean master.ec2.internal 2>&1 > /dev/null"
        end.join(";")
        "if [ -f '/usr/bin/puppetcleaner' ]; then /usr/bin/env puppetcleaner; else #{str}; fi"
      end
      
      def restart_puppetd
        <<-EOS
echo "Running puppet manifest"
/bin/sh /usr/bin/puppetrerun
        EOS
      end
    end
  end
end