require File.dirname(__FILE__) + "/remoter"

module PoolParty  
  module Remote
    
    class RemoteInstance
      include Remote
      
      attr_reader :ip, :name
      
      def initialize(opts={})
        @ip = opts[:ip]
        @name = opts[:name]
      end
      
      # Determine if the RemoteInstance is responding
      def responding?
        true        
      end
      
      # This is how we get the current load of the instance
      # The approach of this may change entirely, but the usage of
      # it will always be the same
      def load
        0.5
      end
      
      # Printing. This is how we extract the instances into the listing on the 
      # local side into the local listing file
      def to_s
        "#{@name} #{@ip} #{responding?} #{load}"
      end
    end
    
  end  
end