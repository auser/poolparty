module PoolParty
  module Remote
    
    module EC2
      def launch_new_instance!
        true
      end
      # Terminate an instance by id
      def terminate_instance!(id=nil)
        true
      end
      # Describe an instance's status
      def describe_instance(id=nil)
        []
      end
      def instances_list
        []
      end      
    end
        
  end
  
  register_remote_base :EC2
end