module PoolParty
  module Resources
    
    class Mount < Resource
      
      default_options({
        :mountpoint => "/data",
        :remounts => "true",
        :mount_options => "rw,nosuid,noquota",
        :fstype => "xfs",
        :atboot => "yes"
      })
      
      def disallowed_options
        [:name]
      end
      
    end
    
  end
end