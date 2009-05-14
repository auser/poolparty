module PoolParty  
  module Remote
    
    class MetavirtInstance
      attr_reader :ip, :mac_address, :vmx_file, :keypair, :cloud
      
      def initialize(o={}, cld=nil)
        @ip = o[:ip]
        @keypair = o[:keypair]        
        @cloud = cld
      end
      
      def to_hash
        {
          :status => status,
          :mac_addresses => mac_address,
          :ip => ip,
          :instance_id => vmx_file,
          :internal_ip => ip,
          :keypair => keypair
        }
      end
      
      def status
        "running"
      end      
      # Is this instance running?
      def running?
        true
      end
      # Is this instance pending?
      def pending?
        false
      end
      # Is this instance terminating?
      def terminating?
        false
      end
      # Has this instance been terminated?
      def terminated?
        false
      end
      
      def launch!
      end
      def terminate!(o)
      end
      
    end    
    
  end
end