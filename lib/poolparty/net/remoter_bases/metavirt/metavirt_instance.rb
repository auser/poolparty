module PoolParty
  module Remote
    
    class MetavirtInstance < RemoteInstance
      dsl_methods :mac_address, 
                  :vmx_file
      
      def to_hash
        {
          :status         => status,
          :mac_addresses  => mac_address,
          :ip             => ip,
          :instance_id    => instance_id,
          :internal_ip    => internal_ip,
          :keypair_name   => keypair.basename
        }
      end
      
      def keypair(n=nil)
        if n.nil?
          @keypair ||= Key.new(keypair_name)
        else
          @keypair = Key.new(n)
        end
      end
      
      def launch!
      end
      
      def terminate!
        `virsh destroy #{instance_id}`
      end
      
    end
    
  end
end