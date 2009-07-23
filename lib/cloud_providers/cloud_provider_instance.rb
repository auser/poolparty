module CloudProviders
  
  class CloudProviderInstance
      include Dslify
      # include Enumerable
      include Connections
      
      default_options(
        :name         => nil, # Name of the remote instance (internal usage)
        :internal_ip  => nil, # Internal ip of the remote instance
        :public_ip    => nil,
        :dns_name     => nil,
        :status       => nil, # Status of the remote instance
        :launch_time  => nil,
        :keypair_name => nil,
        :cloud_name   => nil
      )
      
      def initialize(opts={}, &block)
        self.cloud_name= opts[:cloud].name if opts[:cloud]
        set_vars_from_options(opts)
        instance_eval(&block) if block
        loaded
      end
      
      # Returns an instance of Keypair
      # You can pass either a filename which will be searched for in ~/.ec2/ and ~/.ssh/
      # or you can pass a full filepath
      def keypair(n=keypair_name)
        cloud_provider.keypair(n)
      end
      
      def cloud_provider(opts={}, &block)
        raise StandardError.new("cloud_provider has not been implemented for this CloudProviderInstance ")
      end
      
      ## hash like methods
      def each
        dsl_options.each{ |k,v| yield k,v }
      end
      
      def [](k)
        if dsl_options.has_key? k
          dsl_options[k]
        else
          nil
        end
      end
      
      def []=(k,v)
        dsl_options[k] = v
      end
      
      def has_key?(key)
        dsl_options.has_key?(key)
      end
      
      def keys
        dsl_options.keys
      end
         
      def values
        dsl_options.values
      end
      
      def to_hash
        dsl_options
      end
      ##end of hash like methods
      
      # Is this instance running?
      def running?
        !(status =~ /running/).nil?
      end
      # Is this instance pending?
      def pending?
        !(status =~ /pending/).nil?
      end
      # Is this instance terminating?
      def terminating?
        !(status =~ /shutting/).nil?
      end
      # Has this instance been terminated?
      def terminated?
        !(status =~ /terminated/).nil?
      end
      
      def elapsed_runtime
        Time.now.to_i - launching_time.to_time.to_i
      end
      
      # Callback
      def loaded
      end
      
      def cloud(n=nil)
        @cloud ||= n
      end
      
      # The instances is only valid if there is an internal_ip and a name
      def valid?
        (internal_ip.nil? || name.nil?) ? false : true
      end
      
    end
    
end
