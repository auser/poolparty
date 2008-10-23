module PoolParty
  module Provisioner
    class Slave < ProvisionerBase

      def install_tasks
        [ 
          setup_puppet
        ] << configure_tasks
      end

      def configure_tasks
        [
          setup_configs,
          start_puppet,
          start_poolparty_messenger
        ]
      end

      def setup_puppet
        <<-EOE
          if [ -z "$(grep -v '#' /etc/hosts | grep 'master')" ]; then echo "#{master_ip} puppet master" >> /etc/hosts; else echo "host already set"; fi
          cp #{Base.remote_storage_path}/namespaceauth.conf /etc/puppet/namespaceauth.conf
          echo 'DAEMON_OPTS="-w 120 –fqdn #{@instance.name} –server master"' > /etc/default/puppet
        EOE
      end

      def setup_configs
        <<-EOS          
          echo "#{open(File.join(template_directory, "puppet.conf")).read}" > /etc/puppet/puppet.conf        
        EOS
      end

      # /etc/init.d/puppetmasterd stop
      # puppetd --listen --fqdn #{@instance.name}
      def start_puppet
        <<-EOS
          ps aux | grep "puppetmasterd" | awk '{print $2}' | xargs kill
          rm -rf /etc/puppet/ssl*
          puppetd --test  2>&1 &
        EOS
      end
      
      def master_ip
        @cloud.master.ip
      end

    end
  end
end