module PoolParty
  module Remote
    
    class LibvirtInstance# < Remote::MetavirtInstance
      include Dslify
      
      dsl_methods :name,        # Name of the remote instance (internal usage)
                  :instance_id, # Libvirt UUID
                  :image_id,
                  :ip,          # Ip of the remote instance, by default, the public_ip
                  :internal_ip, # Internal ip of the remote instance
                  :public_ip,
                  :status,      # Status of the remote instance
                  :mac_address,
                  :keypair_name,
                  :cloud
      
      def initialize(o={}, cld=nil)
        set_vars_from_options o
        super
      end
      
      def to_hash
        dsl_options
      end
      
      def details
        xml = `virsh dumpxml #{instance_id || image_id}`
        hsh = XmlSimple.xml_in(xml, 'KeyToSymbol'=>true)
        hsh[:state] = `virsh domstate #{instance_id || image_id}`
        hsh
      end
      
      def mac_address
        
      end
      
    end
    
  end
end