module PoolParty    
  module Resources
    
    def file(opts={}, &block)
      resource(:file) << PoolParty::Resources::File.new(opts, &block)
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