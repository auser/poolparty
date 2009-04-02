module PoolParty  
  module Remote
    
    class VmwareInstance
      attr_accessor :ip, :mac_address, :vmx_file
      
      def initialize(opts={})
        @status = opts[:status] || 'running'
        @ip = opts[:ip]
        @vmx_file = opts[:vmx_file]
        # super
      end
      
      def my_cloud
        Vmrun.new
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