=begin rdoc
  The Vmrun remote base calls out to for Remote Bases
  
  By extending this class, you can easily add remoters to 
  PoolParty. There are 4 methods that the remote base needs to implement
  in order to be compatible.
  
  The four methods are:
    launch_new_instance!
    terminate_instance(id)
    describe_instance(id)
    describe_instances
  
  After your remote base is written, make sure to register the base outside the context
  of the remote base, like so:
    register_remote_base :remote_base_name
  
=end

module PoolParty    
  module Remote
    class Vmrun < Remote::RemoterBase
      include Dslify

      # FIXME: replace this hash with a real method to determine the ip using arp
       DEFAULT_NETWORK_ADDRESSES = {'00:0c:29:0d:39:2e'=> '192.168.4.104',
                                    '00:0c:29:2e:ad:db'=> '172.16.68.130'}
                                          
      default_options(
        :path => 'vmrun',
        :images_repo_path => ::File.expand_path("/Documents/Virtual_Machines.localized/"),
        :default_cli_options => 'gui',
        :terminate_options => 'soft',
        :vmx_file => 'need to specify a vmx_file',
        :network_addresses => DEFAULT_NETWORK_ADDRESSES
      )

      def initialize(parent=nil, opts={}, &block)
        dsl_options opts
        name = ::File.basename(opts[:vmx_file], '.vmx') if opts[:vmx_file]
        super(parent, &block)
      end
      
      #terminate all running instances
      def self.terminate!(o={})
        Vmrun.describe_instances(o).each do |vmxf|
           Vmrun.terminate_instance! vmxf
        end
      end

      def self.launch_new_instance!(o={})
        Vmrun.new(o).launch_new_instance(o)
      end
      def launch_new_instance!(o={})
        puts cmd = "#{path} start #{vmx_file}"
        after_launched if run_local cmd
      end

      # Terminate an instance by id
      def self.terminate_instance!(o)
        raise "You must provide a valid vmx path"  if !o[:vmx_file] || !::File.exists?(o[:vmx_file])
        `#{path} stop #{o[:vmx_file]}`
      end
      def terminate_instance!(o)
        dsl_options(o)
        before_shutdown
        run_local("#{path} stop #{vmx} #{terminate_options} ")
      end

      # Describe an instance's status, must pass :vmx_file in the options
      def self.describe_instance(_vmx_file=nil, o={})
        vmx_file = _vmx_file || o[:vmx_file] || Vmrun.describe_instances.first
        p vmx_file
        Vmrun.new( o.merge(:vmx_file=>_vmx_file) ).describe_instance(_vmx_file)
      end
      def describe_instance(o={})
        {:running? => run_local("#{path} list").grep( ::File.expand_path(vmx_file) ).empty?,
        :mac_addresses => mac_addresses,
        :ip => ip}
      end

      def self.describe_instances(o={})
        output = run_local "#{default_options.merge(o).path} list"
        lines = output.split("\n")
        lines.shift
        lines #todo, reuturn array of vmrun_instances
      end
      def describe_instances(o={})
        self.class.describe_instances(o)
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

      # vmrun specific methods
      def self.run_local(cmd, o={:raise_on_error=>false, :verbose=>true})
        puts "run_local => #{cmd}" if o[:verbose]
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

      #FIXME: this should query the network give a mac address, for now, we will mock it  
      def ip
        @ip ||= network_addresses.values_at(mac_addresses).pop
        # result = run_local("arp -a|grep #{mac_addresses}").match(/\((\w+:.*)\)/)
        # result[1] if result  #TODO  
      end
      def mac_addresses
        @mac_address ||= vmx_data.match(/ethernet\d{1,2}.generatedAddress = "(\w+:.*)"/)[1]
      end
      def vmx_data
        @vmx_data ||= open(vmx_file).read
      end
      def id(vfile)
        vmx_file(vfile)
      end

      ## method's to override default RemoteInstance
      def instance_id
        vmx_file
      end

      
    end
  end
end
