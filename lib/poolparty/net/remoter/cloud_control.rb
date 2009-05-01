require "ping"

module PoolParty
  module Remote
    
    # # Are too few instances running?
    def are_too_few_instances_running?
      nodes(:status => "running").size < minimum_instances.to_i
    end
    # # Are there more instances than allowed?
    def are_too_many_instances_running?
      nodes(:status => "running").size > maximum_instances.to_i
    end

    def list_of_nodes_exceeding_minimum_runtime
      nodes(:status => "running").reject{|i| i.elapsed_runtime < minimum_runtime}
    end
    
    def are_any_nodes_exceeding_minimum_runtime?
      !list_of_nodes_exceeding_minimum_runtime.blank?
    end
    
  end
end