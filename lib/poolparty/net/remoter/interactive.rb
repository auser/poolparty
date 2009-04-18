module PoolParty
  module Remote
    # The following methods are inherent on the RemoterBase
    # If you need to overwrite these methods, do so with caution
    # Listing methods
    def list_of_running_instances(list = list_of_nonterminated_instances)
      list
    end
    # Get a list of the pending instances
    def list_of_pending_instances(list = remote_instances_list)
      list.select_with_hash({:status => "pending"})
    end
    # list of shutting down instances
    def list_of_terminating_instances(list = remote_instances_list)
      list.select_with_hash({:status => "terminated"})
    end
    # Get the instances that are non-master instances
    def nonmaster_nonterminated_instances(list = list_of_nonterminated_instances)
      list_of_nonterminated_instances.reject {|i| i.master? }
    end
    # list all the nonterminated instances
    def list_of_nonterminated_instances(list = remote_instances_list)
      list.select_with_hash({:status => "running"})
    end
    
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
    
    # Get instance by number
    def get_instance_by_number(i=0, list = remote_instances_list)      
      name = ((i.nil? || i.zero?) ? "master" : "node#{i}")
      list.select {|i| i.name == name }.first
    end
    # A callback before the configuration task takes place
    def before_configuration_tasks        
    end
    
    def remote_instances_list        
      @containing_cloud = self
      n = Neighborhoods.load_default
      @remote_instances_list ||= (n ? n.instances : list_of_instances(keypair.basename))
    end

    # List the instances for the current key pair, regardless of their states
    # If no keypair is passed, select them all
    def list_of_instances(keyp=nil)
      tmp_key = (keyp ? keyp : nil)
      
      unless @describe_instances
        tmpInstanceList = remote_base.describe_instances(options).select {|a| a if (tmp_key.nil? || tmp_key.empty? ? true : a[:keypair] == tmp_key) }
        has_master = !tmpInstanceList.select {|a| a[:name] == "master" }.empty?          
        if has_master
          @describe_instances = tmpInstanceList
        else
          @id = 0
          running = select_from_instances_on_status(/running/, tmpInstanceList)
          pending = select_from_instances_on_status(/pending/, tmpInstanceList)
          terminated = select_from_instances_on_status(/shutting/, tmpInstanceList)
          
          running = running.map do |inst|
            inst[:name] = (@id == 0 ? "master" : "node#{@id}")
            @id += 1
            inst
          end.sort_by {|a| a[:index] }
          
          @describe_instances = [running, pending, terminated].flatten
        end
      end
      @describe_instances
    end
    # Select the instances based on their status
    def select_from_instances_on_status(status=/running/, list=[])
      list.select {|a| a[:status] =~ status}
    end

    # Helpers
    def create_keypair
    end
    # Reset the cache of descriptions
    def reset_remoter_base!
      @describe_instances = nil
    end
    def self.included(other)
      # PoolParty.register_remote_base(self.class.to_s.downcase.to_sym)
    end
    
    # Callback after loaded
    def loaded_remoter_base        
    end
    
    # Custom minimum runnable options
    # Extend the minimum runnable options that are necessary
    # for poolparty to run on the remote base
    def custom_minimum_runnable_options
      []
    end
          
    # Custom installation tasks
    # Allow the remoter bases to attach their own tasks on the 
    # installation process
    def custom_install_tasks_for(a=nil)
      []
    end
    # Custom configure tasks
    # Allows the remoter bases to attach their own
    # custom configuration tasks to the configuration process
    def custom_configure_tasks_for(a=nil)
      []
    end
    
  end
end