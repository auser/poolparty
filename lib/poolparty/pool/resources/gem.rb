module PoolParty    
  module Resources
    
    def gem(opts={}, parent=self, &block)
      add_resource(:package, opts.merge({
        :provider => "gem",
        :requires => "Package[rubygems]"
      }), parent, &block)
    end
    
    add_has_and_does_not_have_methods_for(:gem)
    
  end
end