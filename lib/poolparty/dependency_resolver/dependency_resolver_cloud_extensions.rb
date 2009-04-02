=begin rdoc
  Cloud extensions for the DependencyResolver
=end

module PoolParty
  # Take the cloud dependency tree
  module DependencyResolverCloudExtensions    
    def to_properties_hash
      oh = OrderedHash.new
      oh[:options] = options.merge(:cloud_name => name)
      oh[:resources] = resources.keys.inject(OrderedHash.new) do |sum,k|
        sum.merge(k.to_sym => resources[k].map {|a| a.to_properties_hash } )
      end
      oh[:services] = services.keys.inject(OrderedHash.new) do |sum,k|
        sum.merge(k.to_sym => services[k].map {|a| a.to_properties_hash } )
      end
      oh
    end
    
  end
  
  # Adds the to_properties_hash method on top of resources, the lowest level
  module DependencyResolverResourceExtensions
    def to_properties_hash
      options
    end
  end
end