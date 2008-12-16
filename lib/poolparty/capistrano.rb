module PoolParty
  module Capistrano
    
    def set_cloud(cld=nil)
      raise unless cld
      @cloud = cld
    end
        
  end
end