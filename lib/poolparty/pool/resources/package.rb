module PoolParty    
  module Resources
    
    def package(opts={}, &block)
      returning PoolParty::Resources::Package.new(opts, &block) do |r|
        resources << r
      end
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