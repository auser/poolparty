module PoolParty
  module Remote
    
    module Ec2
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
        puts "Here?!?!?!?! in EC2"
        []
      end      
    end
        
  end
  
  register_remote_base :Ec2
end