module PoolParty
  class Base
    plugin :ruby do
      
      def enable                
        has_package(:name => "libreadline-ruby1.8")
        has_package(:name => "libruby1.8")                
        has_package(:name => "ruby1.8-dev")
        has_package(:name => "ruby1.8")
        has_package(:name => "rubygems")
      end
      
      def enable_ri
        has_package(:name => "ri1.8")
      end
      
      def enable_irb
        has_package(:name => "irb1.8")
      end
      
      def enable_rdoc
        has_package(:name => "rdoc1.8")
      end
      
    end  
  end
end