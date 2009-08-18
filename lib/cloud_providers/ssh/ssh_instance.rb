module CloudProviders
  
  class SshInstance < CloudProviderInstance
      
      default_options(
        :user     => 'root',
        :status   => 'running',
        :hostname => nil,
        :cloud    => nil
      )
      
      def instance_id
        self.name
      end
      
      def initialize(opts={}, &block)
        @host = opts[:name] || name || dns_name
        super
      end
      
      def cloud_provider(o={}, &block)
        @cloud_provider ||= if cloud
          cloud.cloud_provider
        else
          options_for_cloud_provider = o.choose{|k,v| Ssh.default_options.has_key?(k)}
          Ssh.new( options_for_cloud_provider, &block)
        end
      end
      
      def refresh!
        run 'uptime'
        name(run('hostname'))
        dns_name(name)
        status('running') if name
        # :internal_ip  => nil, # TODO
        # :public_ip    => nil, #TODO
        self
      end
    
    end
end
