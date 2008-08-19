module PoolParty    
  module Resources
    
    def service(opts={}, &block)
      returning PoolParty::Resources::Service.new(opts, &block) do |r|
        resources << r
      end
    end
    
    class Service < Resource
      
      default_options({
        :besure => "running",
        :enable => "true",
        :name => nil,
        :require => nil
      })      
      
    end
    
  end
end