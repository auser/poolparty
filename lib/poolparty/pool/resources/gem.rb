module PoolParty    
  module Resources
    
    def gem(opts={}, &block)
      resource(:package) << PoolParty::Resources::Package.new(opts.merge({
        :provider => "gem",
        :requires => "Package[rubygems]"
      }), &block)
    end
    
    add_has_and_does_not_have_methods_for(:gem)
    
  end
end