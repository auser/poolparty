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
require "open3"

require "#{File.dirname(__FILE__)}/vmware_instance"

module CloudProviders
  class Vmware < CloudProvider
    include SearchablePaths
    
    has_searchable_paths(:paths => [
      File.expand_path("/Library/Application Support/VMware Fusion"),
      File.expand_path("#{ENV["HOME"]}/Library/Application Support/VMware Fusion"),
      "/usr/bin"
    ])
    
    default_options(
      :image_id             => nil,
      :public_ip            => nil,
      :default_cli_options  => 'gui',
      :terminate_options    => 'soft',
      :keypair_name         => nil,
      :images_repo_path     => ::File.expand_path("#{ENV["HOME"]}/Documents/Virtual_Machines.localized/")
      )
    
    attr_reader :cloud
    
    def initialize(o={}, &block)
      @cloud = o.delete(:cloud)
      super
    end
    
    # Start a new instance with the given options
    def run_instance(o={})
      vmrun("start #{vmx_file}")
      vmware_instance
    end
    
    # Will select the first instance matching the provided criteria hash
    def describe_instance(hash_of_criteria_to_select_instance_against)
      describe_instances.first
    end
    
    # Describe instances
    def describe_instances(o={})
      output = vmrun("list")
      if output.first =~ /Total running VMs: 0/
        []
      else
        output.shift if output.first =~ /Total running/
        output.map do |line|
          vmware_instance(:vmx_file => line)
        end
      end
    end
    
    # Terminate an instance (or instances) by passing :instance_id and :instance_ids
    def terminate_instance!(o={})
      set_vars_from_options o
      vmrun("stop #{vmx_file}")
      describe_instances
    end
    
    # The nodes in the cloud_provider
    def nodes(hsh={})
      describe_instances
    end
    
    # Basic vmware_instance
    def vmware_instance(o={})
      VmwareInstance.new( :instance_id => (o[:vmx_file] || vmx_file), :public_ip => public_ip, :dns_name => public_ip,
                          :cloud_provider => self.dsl_options, :keypair_name => keypair.basename)
    end
    
    # Search for the vmrun binary
    def path_to_binary
      @path_to_binary ||= search_in_known_locations("vmrun")
    end
    
    # Run vmrun on the command-line
    def vmrun(cmd, o={})
      stdin, stdout, stderr = Open3.popen3("'#{path_to_binary}' #{cmd}")
      unless $?.success?
        $stderr.puts "FAILED: #{cmd}\n code = #{$?}"
        raise StandardError.new("ERROR: vmrun") if o.delete(:raise_on_error)
      end
      stdout.readlines
    end
        
    # Search for the vmx_file
    def vmx_file
      return @vmx_file if @vmx_file
      o = if File.file?(f = File.expand_path(image_id))
        f
      elsif File.file?(f = File.expand_path(images_repo_path, image_id))
        f
      end
      @vmx_file = "'#{o}'"
    end

    def before_compile(args); end
    def after_compile(args); end
    
  end
end