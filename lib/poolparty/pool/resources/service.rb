module PoolParty    
  module Resources
    
    def service(opts={}, &block)
      resources[:service] ||= PoolParty::Resources::Service.new(opts, &block)
    end
    
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