module PoolParty
  module CloudResourcer
    
    # Set instances with a range
    def instances(arg)
      if arg.is_a?(Range)
        minimum_instances arg.first
        maximum_instances arg.last
      end
    end
    
  end
end