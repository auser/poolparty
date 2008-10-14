module PoolParty
  module Provisioner
    class Slave < ProvisionerBase

      def install_tasks
        [
          install_puppet,        
          setup_puppet
        ] << configure_tasks
      end

      def configure_tasks
        [
          setup_configs,
          start_puppet
        ]
      end

      def install_puppet
        <<-EOE        
          #{installer_for( puppet_packages )}
          echo 'DAEMON_OPTS="-w 120 --server puppet"' > /etc/default/puppet
        EOE
      end

      def setup_puppet
        <<-EOE
          puppetd --mkusers
          if [ -z "$(grep -v '#' /etc/hosts | grep 'puppet')" ]; then echo "#{master_ip} puppet" >> /etc/hosts; else echo "host already set"; fi
          mv #{Base.remote_storage_path}/#{Base.tmp_path}/namespaceauth.conf /etc/puppet/namespaceauth.conf
        EOE
      end

      def setup_configs
        <<-EOS
          echo "#{open(File.join(template_directory, "puppet.conf")).read}" > /etc/puppet/puppet.conf        
        EOS
      end

      def start_puppet
        <<-EOS
puppetd --listen --fqdn=#{@instance.name}
        EOS
      end
      
      def master_ip
        @cloud.master.ip
      end

    end
  end
end