# Get the names of the nodes. Mainly used for puppet templating
module PoolParty
  module Remote
    def list_of_node_names(options={})
      list_of_running_instances.collect {|ri| ri.name }
    end
    # An array of node ips. Mainly used for puppet templating
    def list_of_node_ips(options={})
      list_of_running_instances.collect {|ri| ri.ip }
    end
  end
end