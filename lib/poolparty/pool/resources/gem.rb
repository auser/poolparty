module PoolParty    
  module Resources
    
    def gem(opts={}, &block)
      resource(:package) << PoolParty::Resources::Package.new(opts.merge({
        :provider => "gem",
        :requires => "Package[rubygems]"
      }), &block)
    end
    
  end
end