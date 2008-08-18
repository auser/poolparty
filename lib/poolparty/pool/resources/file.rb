module PoolParty    
  module Resources
    
    def file(opts={}, &block)
      returning PoolParty::Resources::File.new(opts, &block) do |r|
        resources << r
      end
    end
    
    class File < Resource
      
      default_options({
        :ensure => "present",
        :mode => 644,
        :owner => "poolparty",
        :name => nil
      })
      
    end
    
  end
end