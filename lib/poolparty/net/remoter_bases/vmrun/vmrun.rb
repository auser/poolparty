=begin rdoc
  The Vmrun remote base uses the vmrun command to implement a cloud remoter base.
  The Vmrun remoter base has been tested with Vmware fusion on the mac.
  
  In order to use the Vmrun remoter base you will need to setup a few things.
  First, you will need to have the Vmware fusion installed and vmrun command in your path.
  The default location of the vmrun binary is /Library/Application Support/VMware Fusion/vmrun.
  You will then of course need a virtual machine installed and available.  
  Once you have your instance installed and running, you need to setup your ssh keys so that poolparty can ssh into the instance. Get the ip of you instnace with ifconfig inside the running instance.
  For example:
    
    ssh root@172.0.1.129 "mkdir /root/.ssh && chmod 600 /root/.ssh"
    scp my_key.pub root@172.0.1.129:/root/.ssh/authorized_keys
    
  provide a using :vmrun block in your clouds.rb
  for example:
    
    using :vmrun do
      vmx_hash(::File.expand_path("~/Documents/Virtual\ Machines.localized/Ubuntu-jaunty.vmwarevm/Ubuntu-jaunty.vmx") => '172.16.68.129')
    end
   
   The vmx file fulfills a similar purpose as the ami id in ec2. Note that expand path.  vmrun return fuill paths, so we must provide full paths so things match up later. W
   Vmrun does not provide a meta server like ec2 has, so you need to list the ip address of your VM.  For this reason it is recommended that you use NAT addressing on your VM to maintain consistent addressing across different physical networks.
  
  Also, note that vmrun does not copy the VM to a new distinct VM on each run, so if you want to be able to start from a known state, you should make a snapshot before using your vm with poolparty.  Then, you can rollback to this initial state if you want to ensure you can repeat a fresh cloud-start.
  
=end

module PoolParty
  module Remote
    class Vmrun < Remote::RemoterBase
      include Dslify

      default_options(
        :path_to_binary      => 'vmrun',
        :default_cli_options => 'gui',
        :terminate_options   => 'soft',
        :vmx_hash            => {},  # hash of vmx_filename => ip
        :images_repo_path    => ::File.expand_path("~/Documents/Virtual_Machines.localized/")
      )

      def vmx_files(n=nil)
        if n.nil?
          dsl_options[:vmx_files] || vmx_hash.keys
        else
          dsl_options[:vmx_files] = n
        end
      end

      def self.launch_new_instance!(o={})
        # puts "launch_new_instance 0 = #{o.inspect}"
        new(o).launch_new_instance!
      end
      def launch_new_instance!(o={})
        raise "No available vmx files given!" unless next_unused_vmx_file
        vmx_file = next_unused_vmx_file
        VmwareInstance.new( {:vmx_file  => vmx_file, 
                             :public_ip => ip(vmx_file),
                             :ip => ip(vmx_file),
                             :keypair => keypair
                            }.merge(o)
                          ).launch!
      end
      # Terminate an instance by id
      def self.terminate_instance!(o={})
        new(o).terminate_instance!
      end
      def terminate_instance!(o={})
        set_vars_from_options o
        VmwareInstance.new( :vmx_file => last_unused_vmx_file, 
                            :keypair => keypair
                          ).terminate!(terminate_options)
      end

      # Describe an instance's status, must pass :vmx_file in the options
      def self.describe_instance(o={})
        # vmx_file = o[:vmx_file] || Vmrun.running_instances.first
        new(o).describe_instance
      end
      def describe_instance(o={})        
        running_instances.select {|inst| inst.vmx_file == o[:vmx_file] }.first
      end

      def self.describe_instances(o={})
        new(o).describe_instances
      end
      def describe_instances(o={})
        # TODO: WTF
        running_instances.map {|a| a.to_hash.merge(:public_ip => ip(a.vmx_file), :ip => ip(a.vmx_file)) }
      end

      # After launch callback
      # This is called after a new instance is launched
      def after_launched(force=false)
        puts "new vmware instance was launched"
      end

      # Before shutdown callback
      # This is called before the cloud is contracted
      def before_shutdown
      end
      def self.path_to_binary
        new({}).path_to_binary
      end
            
      def after_launch_instance(inst=nil)
        if inst
          dputs "Associate address after launched: #{inst.public_ip}"
        end
      end
      
      private
      
      def running_instances(o={})
        output = run_local "#{path_to_binary} list"
        lines = output.split("\n")
        lines.shift
        lines.map {|vmx_file| VmwareInstance.new( :vmx_file => vmx_file, 
                                                  :ip => ip, 
                                                  :keypair => keypair
                                                ) }
      end
      
      # vmrun specific methods
      def self.run_local(cmd, o={:raise_on_error=>false, :verbose=>true})
        output = `#{cmd}`
        unless $?.success?
          $stderr.puts "FAILED: #{cmd}\n code = #{$?}"
          raise "ERROR: run_local" if o[:raise_on_error]
        end
        output
      end
      
      def run_local(cmd, o={:raise_on_error=>false, :verbose=>true})
        self.class.run_local(cmd, o)
      end
      
      def next_unused_vmx_file
        tmp = (vmx_files - running_instances.map {|a| a.vmx_file })
        (tmp.empty? ? vmx_files : tmp).first
      end
      
      def last_unused_vmx_file
        running_instances.last.vmx_file
      end
      
      def id(vfile)
        vmx_file(vfile)
      end
      
      ## method's to override default RemoteInstance
      def instance_id
        vmx_file
      end
      
      def keypair
        cloud.keypair_name
      end
      
      def ip(vmx_file_string=nil)
        return dsl_options[:ip] if dsl_options[:ip]
        vmx_file_string ? vmx_hash[vmx_file_string] : nil
      end
      
    end
  end
end
