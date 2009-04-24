module PoolParty    
  module Resources
        
    class Cron < Resource
      
      default_options({
        :command => nil,
        :user => "root"
      })
      
      def present
        :create
      end
      
      def absent
        :delete
      end

    end
    
  end
end