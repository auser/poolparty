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

      default_options(
        :path_to_binary => 'vmrun',
        :images_repo_path => ::File.expand_path("/Documents/Virtual_Machines.localized/"),
        :default_cli_options => 'gui',
        :terminate_options => 'soft',
        :vmx_file => 'need to specify a vmx_file'
      )
      
      def initialize(parent=nil, opts={}, &block)
        dsl_options opts
        name = ::File.basename(opts[:vmx_file], '.vmx') if opts[:vmx_file]
        instance_eval &block if block
        super(parent, &block)
      end
      
      #terminate all running instances
      def self.terminate!(o={})
        Vmrun.describe_instances(o).each do |vmxf|
           Vmrun.terminate_instance! o.merge(:vmx_file => vmxf)
        end
      end

      def self.launch_new_instance!(o={})
        Vmrun.new(parent, o).launch_new_instance!
      end
      def launch_new_instance!
        cmd = "#{path_to_binary} start \"#{vmx_file}\""
        if run_local cmd        
          describe_instance options.merge({
            :mac_address => mac_address,
            :ip => ip
          })
        end
      end

      # Terminate an instance by id
      def self.terminate_instance!(o)
        Vmrun.new(parent, o).terminate_instance!
      end
      def terminate_instance!(o={})
        dsl_options o
        run_local("#{path_to_binary} stop \"#{vmx_file}\" #{terminate_options} ")
      end

      # Describe an instance's status, must pass :vmx_file in the options
      def self.describe_instance(_vmx_file=nil, o={})
        vmx_file = _vmx_file || o[:vmx_file] || Vmrun.describe_instances.first
        Vmrun.new(parent, o.merge(:vmx_file=>_vmx_file) ).describe_instance(_vmx_file)
      end
      def describe_instance(o={})
        {
          :status => (run_local("#{path_to_binary} list").grep( ::File.expand_path(vmx_file) ).empty? ? "terminated" : "running"),
          :mac_addresses => mac_address,
          :status => "running",
          :ip => ip,
          :internal_ip => ip
          # :keypair => keypair
        }
      end

      def self.describe_instances(o={})
        Vmrun.new(parent, o).describe_instances
      end
      def describe_instances(o={})
        output = run_local "#{path_to_binary} list"
        lines = output.split("\n")
        lines.shift
        lines.each {|vmx_file| describe_instance(o.merge({:vmx_file => vmx_file})) }
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
