require File.dirname(__FILE__) + "/remoter"

module PoolParty  
  module Remote
    
    class RemoteInstance
      include Remote
      include PoolParty::Remoter
      
      attr_reader :ip, :name
      
      def initialize(opts={})
        @ip = opts[:ip]
        @name = opts[:name]
      end
      
      def to_s
        "#{@ip}\t\t#{@name}"
      end
    end
    
  end  
end