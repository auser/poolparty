module PoolParty
  module Remote
    
    # This class is the base class for all remote types
    # Everything remoting-wise is derived from this class
    class RemoterBase
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
      def instances_list
        raise RemoteException.new(:method_not_defined, "instances_list")
      end
    end
    
  end
end