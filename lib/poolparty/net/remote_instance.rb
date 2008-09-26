require File.dirname(__FILE__) + "/remoter"

module PoolParty  
  module Remote
    
    class RemoteInstance
      include Remote
      include PoolParty::Remote::Remoter
      
      attr_reader :ip, :name
      
      def initialize(opts={})
        @ip = opts[:ip]
        @name = opts[:name]
      end
      
      def responding?
        true        
      end
      
      def to_s
        "#{@ip}\t#{@name}\t#{responding?}"
      end
    end
    
  end  
end