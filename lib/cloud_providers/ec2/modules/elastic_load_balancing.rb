module CloudProviders
  module ElasticLoadBalancing
    
    # Create and store the load balancers from args
    # The args are in the format [[name, block], [name, block]]
    def create_load_balancers(args)
      args.each do |arr|
        load_balancers << ElasticLoadBalancer.new(arr)
      end
    end
        
    def load_balancers
      @load_balancers ||= []
    end
    
  end
end