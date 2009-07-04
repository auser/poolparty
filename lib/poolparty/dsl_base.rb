=begin rdoc
  Dsl Base class for all cloud dsl methods
=end

module PoolParty
  class DslBase < Base
    
    default_options(
      :minimum_instances    => 2,
      :maximum_instances    => 5
    )
    
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