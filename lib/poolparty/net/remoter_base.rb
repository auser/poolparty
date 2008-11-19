module PoolParty

  def register_remote_base(*args)
    args.each do |arg|
      (remote_bases << "#{arg}".downcase.to_sym)
    end
  end
  
  def remote_bases
    $remote_bases ||= []
  end

  module Remote    
    # This class is the base class for all remote types
    # Everything remoting-wise is derived from this class
    module RemoterBaseMethods
      # Required methods
      # The next methods are required on all RemoteInstance types
      # If your RemoteInstance type does not overwrite the following methods
      # An exception will be raised and poolparty will explode into tiny little 
      # pieces. Don't forget to overwrite these methods
      # Launch a new instance
      def launch_new_instance!
        raise RemoteException.new(:method_not_defined, "launch_new_instance!")
      end
      # Terminate an instance by id
      def terminate_instance!(id=nil)
        raise RemoteException.new(:method_not_defined, "terminate_instance!")
      end
      # Describe an instance's status
      def describe_instance(id=nil)
        raise RemoteException.new(:method_not_defined, "describe_instance")
      end
      # Get instances
      # The instances must have a status associated with them on the hash
      def describe_instances
        raise RemoteException.new(:method_not_defined, "describe_instances")
      end
      
    end
    module RemoterBase
      # The following methods are inherent on the RemoterBase
      # If you need to overwrite these methods, do so with caution
      # Listing methods
      def list_of_running_instances(list = list_of_nonterminated_instances)
        list.select {|a| a.running? }
      end
      # Get a list of the pending instances
      def list_of_pending_instances(list = list_of_nonterminated_instances)
        list.select {|a| a.pending? }
      end
      # list of shutting down instances
      def list_of_terminating_instances(list = remote_instances_list)
        list.reject {|i| true if !i.terminating? }
      end
      # Get the instances that are non-master instances
      def nonmaster_nonterminated_instances(list = list_of_nonterminated_instances)
        list_of_nonterminated_instances.reject {|i| i.master? }
      end
      # list all the nonterminated instances
      def list_of_nonterminated_instances(list = remote_instances_list)
        list.reject {|i| i.terminating? || i.terminated? }
      end
      # get the master instance
      def master
        get_instance_by_number(0)
      end
      # Get instance by number
      def get_instance_by_number(i=0, list = remote_instances_list)
        name = (i.zero? ? "master" : "node#{i}")
        list.select {|i| i.name == name }.first
      end
      def remote_instances_list
        # puts "> #{@containing_cloud.name} - #{@containing_cloud.class}"
        @containing_cloud = self
        list_of_instances(keypair).collect {|h| PoolParty::Remote::RemoteInstance.new(h, @containing_cloud) }
      end
      # List the instances for the current key pair, regardless of their states
      # If no keypair is passed, select them all
      def list_of_instances(keyp=nil)
        tmp_key = (keyp ? keyp : keypair).to_s
        unless @describe_instances
          tmpInstanceList = describe_instances.select {|a| a if tmp_key ? a[:keypair] == tmp_key : true }
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
        reset_base!
      end
      def self.included(other)
        PoolParty.register_remote_base(self.class.to_s.downcase.to_sym)
      end
      
      # Callback after loaded
      def loaded_remoter_base        
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
end

Dir["#{File.dirname(__FILE__)}/remote_bases/*.rb"].each {|base| require base }