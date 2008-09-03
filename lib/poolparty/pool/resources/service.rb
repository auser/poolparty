module PoolParty    
  module Resources
    
    def service(opts={}, &block)
      resource(:service) << PoolParty::Resources::Service.new(opts, &block)
    end
    
    add_has_and_does_not_have_methods_for(:service)
    
    class Service < Resource
      
      default_options({
        :ensure => "running",
        :enable => "true",
        :name => nil,
        :require => nil
      })
           
    end
    
  end
end