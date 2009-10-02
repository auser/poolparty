module CloudProviders
  module ElasticLoadBalancing
    
    def load_balancers
      @load_balancers ||= []
    end
    
    def load_balancer_names
      load_balancers.map {|a| a.name }
    end
    
    def create_load_balancer(array)
      name, opts, block = array
      load_balancers << ElasticLoadBalancer.new(name, opts, &block) unless load_balancer_names.include?(name)
    end
    
  end
end