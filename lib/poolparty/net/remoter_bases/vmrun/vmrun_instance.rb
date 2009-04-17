module PoolParty  
  module Remote
    
    class VmwareInstance
      attr_accessor :ip, :mac_address, :vmx_file
      
      def initialize(vmx_file=nil)
        @ip = opts[:ip]
        @vmx_file = opts[:vmx_file]
        dsl_options opts        
      end
      
      
      def status
        "running"
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
      def ip
        @ip ||= %x[arp -a].select {|a| a if a =~ /#{mac_address.macify}/}.first[/(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})/]
      end
      def mac_address
        @mac_address ||= parse_vmx_file[:"ethernet0.generatedAddress"]
      end
      def vmx_data
        @vmx_data ||= open(vmx_file).read
      end
      def parse_vmx_file
        vmx_data.to_hash
      end
      
    end    
    
  end
end