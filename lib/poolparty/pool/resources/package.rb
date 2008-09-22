module PoolParty    
  module Resources
    
    def package(opts={}, &block)
      resource(:package) << PoolParty::Resources::Package.new(opts, &block)
    end
    
    add_has_and_does_not_have_methods_for(:package)
    
    class Package < Resource
      
      default_options({
        :ensure => "installed",
        :name => nil
      })
      
      
    end
    
  end
end