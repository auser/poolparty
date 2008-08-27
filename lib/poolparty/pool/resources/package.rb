module PoolParty    
  module Resources
    
    def package(opts={}, &block)
      resource(:package) << PoolParty::Resources::Package.new(opts, &block)
    end
    
    class Package < Resource
      
      default_options({
        :ensure => "installed",
        :alias => nil,
        :name => nil
      })
      
      
    end
    
  end
end