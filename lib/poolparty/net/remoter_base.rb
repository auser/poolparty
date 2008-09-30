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
        list.select {|a| a[:status] =~ /running/}
      end
      # Get a list of the pending instances
      def list_of_pending_instances(list = list_of_nonterminated_instances)
        list.select {|a| a[:status] =~ /pending/}
      end
      # list of shutting down instances
      def list_of_terminating_instances(list = list_of_nonterminated_instances)
        list.select {|a| a[:status] =~ /shutting/}
      end
      # list all the nonterminated instances
      def list_of_nonterminated_instances(list = list_of_instances)
        list.reject {|a| a[:status] =~ /terminated/}
      end
      # List the instances for the current key pair, regardless of their states
      # If no keypair is passed, select them all
      def list_of_instances(keypair=nil)
        describe_instances.select {|a| keypair ? a[:keypair] == keypair : a}
      end
      def instances_list
        list_of_instances.map {|i| PoolParty::Remote::RemoteInstance.new(i)}
      end
      # Instances
      # Get the master from the cloud
      def master
        @list = list_from_remote
        @list.reject {|a| a unless a.name =~ /master/ }.first if @list.class != String
      end
      def self.included(other)
        PoolParty.register_remote_base(self.class.to_s.downcase.to_sym)
      end
      
    end
    
  end
end

Dir["#{File.dirname(__FILE__)}/remote_bases/*.rb"].each {|base| require base }