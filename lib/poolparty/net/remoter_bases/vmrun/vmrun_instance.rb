module PoolParty  
  module Remote
    
    class VmwareInstance
      
      def initialize(opts={}, prnt=Ec2.new)
        @uniquely_identifiable_by = [:ip, :name, :dns_name, :instance_id]
        @original_options = opts
        @my_cloud = prnt
        super(opts, prnt)
        find_myself(@uniquely_identifiable_by && opts.keys) if prnt.respond_to?(:describe_instances)
      end
      
      # Is this instance running?
      def running?
        true
      end
      # Is this instance pending?
      def pending?
        false
      end
      # Is this instance terminating?
      def terminating?
        false
      end
      # Has this instance been terminated?
      def terminated?
        false
      end
    end
    
    
  end
end