module PoolParty    
  module Resources
    
    def exec(opts={}, &block)
      resource(:exec) << PoolParty::Resources::Exec.new(opts, &block)
    end
    
    class Exec < Resource
      
      default_options({
        :path => "/usr/bin:/bin:/usr/local/bin"
      })
            
    end
    
  end
end