module PoolParty
  module Remote
    
    class VmwareInstance < RemoteInstance
      dsl_methods :mac_address, :vmx_file, :keypair
      
      def initialize(o={})
        raise "You must pass a vmx_file" unless o[:vmx_file]
        @vmx_file = ::File.expand_path(o[:vmx_file])
        o.delete(:status)
        super o
      end
      
      def to_hash
        { :status       => status,
          :mac_address  => mac_address,
          :ip           => ip,
          :public_ip    => ip,
          :internal_ip  => ip, 
          :instance_id  => vmx_file,
          :vmx_file     => vmx_file,
          :keypair      => keypair
          :key_name     => key_name
        }
      end
      
      def key_name
        @key_name ||= keypair.basename
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
      
      def launch!
        Vmrun.run_local("#{Vmrun.path_to_binary} start \"#{vmx_file}\"")
        dputs "Launched new vmware instance from vmx: #{vmx_file}"
        to_hash
      end
      
      def terminate!(o)
        Vmrun.run_local("#{Vmrun.path_to_binary} stop \"#{vmx_file}\" #{o}")
      end
      
      # Get the ip from the arp -a
      # def ip
      #   @ip ||= %x[arp -a].select {|a| a if a =~ /#{mac_address.macify}/}.first[/(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})/]
      # end
      # Get the mac address in the vmx_file
      def mac_address(mac=nil)
        if mac.nil?
          dsl_options[:mac_address] ||= parse_vmx_file[:"ethernet0.generatedAddress"]
        else
          dsl_options[:mac_address] = mac
        end
      end
      
      def parse_vmx_file
        vmx_data.to_hash
      end
      
      def vmx_data
        @vmx_data ||= open(vmx_file).read
      end
      
    end
    
  end
end