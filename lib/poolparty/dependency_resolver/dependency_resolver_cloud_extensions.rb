=begin rdoc
  Cloud extensions for the DependencyResolver
=end

module PoolParty
  # Take the cloud dependency tree
  module DependencyResolverCloudExtensions    
    def to_properties_hash
      oh = {}
      oh[:options] = options.merge(:cloud_name => name)
      oh[:resources] = ordered_resources.map {|a| a.to_properties_hash }
      # oh[:resources] = resources.keys.inject(OrderedHash.new) do |sum,k|
      #   sum.merge(k.to_sym => resources[k].map {|a| a.to_properties_hash } )
      # end
      # oh[:services] = services.keys.inject(OrderedHash.new) do |sum,k|
      #   sum.merge(k.to_sym => services[k].map {|a| a.to_properties_hash } )
      # end
      oh
    end
    
  end
  
  # Adds the to_properties_hash method on top of resources, the lowest level
  module DependencyResolverResourceExtensions
    def to_properties_hash
      {:pp_type => self.class.to_s.top_level_class}.merge!(options)
    end
  end
end