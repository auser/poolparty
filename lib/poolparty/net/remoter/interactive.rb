module PoolParty
  module Remote
    
    # Select a list of instances based on their status
    def nodes(hsh={})
      # _nodes[hsh] ||= 
      list_of_instances.select_with_hash(hsh)
    end
    
    # Select the list of instances, either based on the neighborhoods
    # loaded from /etc/poolparty/neighborhood.json
    # or by the remote_base on keypair
    def list_of_instances
      return @list_of_instances if @list_of_instances
      @containing_cloud = self
      n = Neighborhoods.load_default
      @list_of_instances = (n.instances.empty? ? _list_of_instances : n.instances)
    end

    private
    # Cache the instances_by_status here
    def _nodes
      @_nodes ||= {}
    end
    
    # List the instances for the current key pair, regardless of their states
    # If no keypair is passed, select them all
    def _list_of_instances(select={})
      @describe_instances ||= remote_base.describe_instances(options).select_with_hash(select)
    end
    
    # If the cloud is starting an instance, it will not be listed in 
    # the running instances, so we need to keep track of the instance
    # that is being started so we can add it to the neighborhood list
    def started_instance
      @started_instance ||= []
    end

    # Reset the cache of descriptions
    def reset_remoter_base!
      @_nodes = @list_of_instances = @describe_instances = nil
    end
    
  end
end