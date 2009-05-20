=begin rdoc
  DependencyResolver
  
  This acts as the interface between PoolParty's dependency tree
  and the dependency providers. To add a new DependencyResolver,
  overload this class with the appropriate calls
=end
module PoolParty
  class DependencyResolver
    
    attr_reader :properties_hash, :the_cloud
    
    def initialize(hsh, contxt={:tmp_path => "/tmp/poolparty/testpool/testcloud"})
      raise DependencyResolverException.new('must pass a hash') if hsh.nil? || !(hsh.instance_of?(OrderedHash) || hsh.instance_of?(Hash))
      @properties_hash = hsh
      @the_cloud = contxt
    end
    
    # Compile the clouds properties_hash into the format required by the dependency resolver
    # This methods should be overwritten by the supclassed methods
    def compile()
      raise "Not Implemented"
    end
    
    def self.compile(hsh, contxt=nil)
      new(hsh, contxt).compile
    end
    
    def self.permitted_resource_options(rules={})
      @permitted_resource_options ||= rules
    end
    
    def permitted_resource_options
      self.class.permitted_resource_options
    end
    
    def tf(count)
      "\t" * count
    end
    
  end
end