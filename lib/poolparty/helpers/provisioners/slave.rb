module PoolParty
  module Provisioner
    class Slave < ProvisionerBase

      def install_tasks
        [ 
          setup_puppet,
          setup_configs
        ] << configure_tasks
      end

      def configure_tasks
        [          
          start_puppet
        ]
      end

      def setup_puppet
        <<-EOE
          if [ -z "$(grep -v '#' /etc/hosts | grep 'master')" ]; then echo "#{master_ip} puppet master" >> /etc/hosts; else echo "host already set"; fi
          cp #{Base.remote_storage_path}/namespaceauth.conf /etc/puppet/namespaceauth.conf
        EOE
      end

      def setup_configs
        <<-EOS          
          echo "#{open(File.join(template_directory, "puppet.conf")).read}" > /etc/puppet/puppet.conf
          /etc/init.d/puppetmaster stop
          # /usr/bin/puppetrerun
        EOS
      end

      # /etc/init.d/puppetmasterd stop
      # puppetd --listen --fqdn #{@instance.name}
      def start_puppet
        <<-EOS
. /etc/profile && /usr/sbin/puppetd --onetime --no-daemonize --logdest syslog --server master  2>&1 > /dev/null
        EOS
      end
      
      def master_ip
        @cloud.master.ip
      end

    end
  end
end