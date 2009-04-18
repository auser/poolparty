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
        :vmx_hash => 'need to specify vmx_files to use'
      )
      
      def initialize(par, opts={}, &block)
        dsl_options opts
        instance_eval &block if block
        super(par, &block)
      end
      
      #terminate all running instances
      def self.terminate!(o={})
        describe_instances(o).each do |vmxf|
           terminate_instance! o.merge(:vmx_file => vmxf)
        end
      end

      def self.launch_new_instance!(o={})
        new_instance.launch_new_instance!
      end
      def launch_new_instance!
        VmwareInstance.new( :vmx_file => next_unused_vmx_file, 
                            :ip => vmx_hash[next_unused_vmx_file], 
                            :keypair => cloud.keypair.basename
                          ).launch!
      end
      # Terminate an instance by id
      def self.terminate_instance!(o={})
        new(nil, o).terminate_instance!
      end
      def terminate_instance!(o={})
        dsl_options o
        VmwareInstance.new( :vmx_file => last_unused_vmx_file, 
                            :ip => vmx_hash[last_unused_vmx_file], 
                            :keypair => parent.keypair.basename
                          ).terminate!(terminate_options)
      end

      # Describe an instance's status, must pass :vmx_file in the options
      def self.describe_instance(o={})
        vmx_file = o[:vmx_file] || Vmrun.running_instances.first
        new_instance.describe_instance(:vmx_file => vmx_file)
      end
      def describe_instance(o={})        
        running_instances.select {|inst| inst.vmx_file == o[:vmx_file] }.first
      end

      def self.describe_instances(o={})
        new_instance.describe_instances
      end
      def describe_instances(o={})
        running_instances.map {|a| a.to_hash }
      end
      def running_instances(o={})
        output = run_local "#{path_to_binary} list"
        lines = output.split("\n")
        lines.shift
        lines.map {|vmx_file| VmwareInstance.new( :vmx_file => vmx_file, 
                                                  :ip => vmx_hash[vmx_file], 
                                                  :keypair => parent.keypair.basename
                                                ) }
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
        new(parent).path_to_binary
      end
      
      private
      def self.new_instance(o={})
        Vmrun.new((cloud rescue o), o)
      end
      
      # vmrun specific methods
      def self.run_local(cmd, o={:raise_on_error=>false, :verbose=>true})
        # puts "Running locally: #{cmd}"
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
        tmp = (vmx_files - running_instances)
        (tmp.empty? ? vmx_files : tmp).first
      end
      
      def last_unused_vmx_file
        running_instances.last.vmx_file
      end
      
      def vmx_files
        vmx_hash.keys
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
