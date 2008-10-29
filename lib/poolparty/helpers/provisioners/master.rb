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
          install_poolparty
        ] << configure_tasks
      end

      def configure_tasks
        [
          # start_puppetmaster,
          create_local_node,
          move_templates,
          create_poolparty_manifest,
          restart_puppetd
        ]
      end
      
      # If the master is not in the hosts file, then add it to the hosts file
      def create_local_hosts_entry
        <<-EOS
if [ -z \"$(grep -v '#' /etc/hosts | grep 'puppet')" ]; then echo '#{@master_ip}           puppet master' >> /etc/hosts; fi
        EOS
      end

      def setup_basic_structure
        <<-EOS
mkdir -p /etc/puppet/manifests/nodes 
mkdir -p /etc/puppet/manifests/classes
echo "import 'nodes/*.pp'" > /etc/puppet/manifests/site.pp
echo "import 'classes/*.pp'" >> /etc/puppet/manifests/site.pp
cp #{Base.remote_storage_path}/namespaceauth.conf /etc/puppet/namespaceauth.conf
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
echo "*" > /etc/puppet/autosign.conf
killall ruby
rm -rf /etc/puppet/ssl/*
puppetmasterd --verbose
        EOS
      end

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
mkdir -p #{Base.template_path}
cp #{Base.remote_storage_path}/#{Base.template_directory}/* #{Base.template_path}
        EOS
      end
      
      def create_poolparty_manifest
        <<-EOS
cp #{Base.remote_storage_path}/poolparty.pp /etc/puppet/manifests/classes/poolparty.pp
cp #{Base.remote_storage_path}/#{Base.key_file_locations.first} "#{Base.base_config_directory}/.ppkeys"
cp #{Base.remote_storage_path}/#{Base.default_specfile_name} #{Base.base_config_directory}/#{Base.default_specfile_name}
#{copy_ssh_app}
        EOS
      end
      
      def copy_ssh_app
        if @cloud.remote_keypair_path != "#{Base.remote_storage_path}/#{@cloud.full_keypair_name}"
          "cp #{Base.remote_storage_path}/#{@cloud.full_keypair_name} #{@cloud.remote_keypair_path}"
        end
      end
      
      def install_poolparty
        <<-EOE
cd /var/poolparty
wget http://rubyforge.org/frs/download.php/44731/logging-0.9.4.gem -O logging.gem
wget http://rubyforge.org/frs/download.php/45581/ZenTest-3.11.0.gem -O zentest.gem
wget http://rubyforge.org/frs/download.php/45600/ParseTree-3.0.1.gem -O parsetree.gem
wget http://rubyforge.org/frs/download.php/45587/ruby2ruby-1.2.0.gem -O ruby2ruby.gem
wget http://rubyforge.org/frs/download.php/45627/activesupport-2.1.2.gem -O activesupport.gem
wget http://rubyforge.org/frs/download.php/18366/xml-simple-1.0.11.gem -O xml-simple.gem
wget http://rubyforge.org/frs/download.php/45683/RubyInline-3.8.1.gem -O RubyInline.gem
wget http://rubyforge.org/frs/download.php/42580/flexmock-0.8.3.gem -O flexmock.gem
wget http://rubyforge.org/frs/download.php/45685/hoe-1.8.2.gem -O hoe.gem
wget http://rubyforge.org/frs/download.php/18698/lockfile-1.4.3.gem -O lockfile.gem
wget http://rubyforge.org/frs/download.php/45546/rubyforge-1.0.1.gem -O rubyforge.gem
wget http://rubyforge.org/frs/download.php/43954/rake-0.8.3.gem -O rake.gem
wget http://rubyforge.org/frs/download.php/45589/sexp_processor-3.0.0.gem -O sexp_processor.gem
wget http://rubyforge.org/frs/download.php/43666/amazon-ec2-0.3.1.gem -O amazon-ec2.gem

#{
  %w(rake lockfile rubyforge hoe zentest sexp_processor flexmock logging activesupport RubyInline parsetree ruby2ruby xml-simple amazon-ec2).map do |dep|
    "gem install -y --no-ri --no-rdoc #{dep}.gem\n"
  end
}
gem sources add http://gems.github.com
gem install -y --no-ri --no-rdoc  --source http://gems.github.com grempe-amazon-ec2
gem install -y --no-ri --no-rdoc  --source http://gems.github.com auser-poolparty
        EOE
      end

      # ps aux | grep puppetmasterd | awk '{print $2}' | xargs kill
      # rm -rf /etc/puppet/ssl
      # puppetmasterd --verbose
      def start_puppetmaster
        <<-EOS
        EOS
      end

      # puppetd --listen --fqdn #{@instance.name}
      def restart_puppetd
        <<-EOS
          . /etc/profile && #{@instance.puppet_runner_command}
        EOS
      end
    end
  end
end