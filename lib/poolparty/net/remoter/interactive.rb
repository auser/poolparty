module PoolParty
  module Remote
    
    # #DEPRECATE We'll stub the ip to be the master ip for ease and accessibility
    # def ip(i=nil)
    #   puts "DEPRECATED:  ip will only be callable against a RemoteInstance in the next release."
    #   i ? options[:ip] = i : (master ? master.ip : options[:ip])
    # end
    # #DEPRECATE: get the master instance
    def master
      puts "DEPRECATED:  'master' is deprecated and will be removed in the next major release."
      get_instance_by_number(0)
    end
    
    # Select a list of instances based on their status
    def nodes(hsh={})
      _nodes[hsh] ||= list_of_instances.select_with_hash(hsh)
    end
    
    # Cache the instances_by_status here
    def _nodes
      @_nodes ||= {}
    end
    
    # Select the list of instances, either based on the neighborhoods
    # loaded from /etc/poolparty/neighborhood.json
    # or by the remote_base on keypair
    def list_of_instances
      @containing_cloud = self
      n = Neighborhoods.load_default
      @list_of_instances ||= (n ? n.instances : _list_of_instances(keypair.basename))
    end

    private
    # List the instances for the current key pair, regardless of their states
    # If no keypair is passed, select them all
    def _list_of_instances(keyp=nil)
      tmp_key = (keyp ? keyp : nil)
      
      @describe_instances ||= remote_base.describe_instances(options).select do |a|
        a if (tmp_key.nil? || tmp_key.empty? ? true : a[:keypair] == tmp_key)
      end
    end
    
    # The instances that this cloud knows about, meaning their neighborhood 
    # and the started instance, if need be
    def all_known_instances
      nodes(:status => "running") + started_instance
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