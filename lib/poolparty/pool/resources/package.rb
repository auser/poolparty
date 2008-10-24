module PoolParty    
  module Resources
        
    class Package < Resource
      
      default_options({
        :ensure => "installed",
        :name => nil
      })
      
      def present
        "installed"
      end
      def absent
        "absent"
      end
      
    end
    
  end
end