=begin rdoc
  Dsl Base class for all cloud dsl methods
  
  Pool and Cloud both are taken from the DslBase so that both
  carry the same dsl. The methods that apply to either/or
  are defined in the respective classes
=end

module PoolParty
  class DslBase < Base
    
    default_options(
      :minimum_instances        => 2,
      :maximum_instances        => 5
    )
    
    # Autoscaling
    def auto_scaling(name=nil, &block)
      _auto_scalers_args << [name, block]
    end
    
    # Load balancers
    # DSL for creating a new load balancer
    def load_balancer(name=nil, opts={}, &block)
      _load_balancers_args << [name, opts, block]
    end
    
    def load_balancers
      cloud_provider.load_balancers
    end
    
    private
    
    def create_load_balancers
      _load_balancers_args.each do |arg_array|
        cloud_provider.create_load_balancer arg_array
      end
    end
        
    def _auto_scalers_args
      @_auto_scaling ||= []
    end
    def _load_balancers_args
      @_load_balancers ||= []
    end
    public
            
    # Set instances with a range or a number
    # if passed with a hash, call nodes(hash) to return filtered list of 
    # instances
    def instances(arg)
      case arg
      when Range
        minimum_instances arg.first
        maximum_instances arg.last
      when Fixnum
        minimum_instances arg
        maximum_instances arg
      when Hash
        nodes(arg)
      else
        raise PoolParty::PoolPartyError.create("DslMethodCall", "You must call instances with either a number, a range or a hash (for a list of nodes)")
      end
    end
    
  end
end