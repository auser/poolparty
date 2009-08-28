require 'socket'
require 'timeout'

module CloudProviders
  
  class CloudProviderInstance
      include Dslify
      # include Enumerable
      include Connections
      include Callbacks
      
      default_options(
        :cloud        => nil, # The cloud this instance belongs to, set automaticly if node is created thru cloud expansion
        :name         => nil, # Name of the remote instance (internal usage)
        :internal_ip  => nil, # Internal ip of the remote instance
        :public_ip    => nil,
        :dns_name     => nil,
        :status       => nil, # Status of the remote instance
        :launch_time  => nil,
        :keypair_name => nil,
        :cloud_name   => nil
      )
      
      def initialize(opts={}, &block)
        self.cloud_name= opts[:cloud].name if opts[:cloud]
        set_vars_from_options(opts)
        instance_eval(&block) if block
        loaded
      end
      
      # Returns an instance of Keypair
      # You can pass either a filename which will be searched for in ~/.ec2/ and ~/.ssh/
      # or you can pass a full filepath
      def keypair(n=keypair_name)
        cloud_provider.keypair(n)
      end
      
      def cloud_provider(opts={}, &block)
        raise StandardError.new("cloud_provider has not been implemented for this CloudProviderInstance ")
      end
      
      # CLOUD PROVIDER METHODS
      
      # Bootstrap self.  Bootstrap runs as root, even if user is set
      def bootstrap!(opts={})
        callback :before_bootstrap
         if !bootstrapped? || opts[:force]
           old_user = user
           @user = "root"
           opts[:os] ||= os || determine_os
           if cloud && cloud.bootstrap_script
             opts[:filename] = cloud.bootstrap_script
           end
          script_file = Provision::Bootstrapper.bootstrap_script(opts)
          scp(:source => script_file, :destination => "/tmp")
          output = run("chmod +x /tmp/determine_os.sh; /bin/sh /tmp/#{File.basename(script_file)}")
          @user = old_user
          output.chomp if output
        end
        callback :after_bootstrap
      end
      
      # Configure the node
      def configure!(opts={})
        ddputs("Configuring: #{self.name}")
        bootstrap! unless bootstrapped?
        callback :before_configure
        raise StandardError.new("You must pass in a cloud to configure an instance") unless cloud
        cloud.compile(self)        
        
        scp(:source       => keypair.full_filepath, 
            :destination  => "/etc/poolparty/keys/#{keypair.basename}")
        
        FileUtils.mkdir_p cloud.tmp_path/"etc"/"poolparty" unless File.directory?(cloud.tmp_path/"etc"/"poolparty")
        pack_clouds_dot_rb_and_expected_directories
        
        dputs("Rsyncing #{cloud.tmp_path/"*"}")
        rsync(:source => cloud.tmp_path/"*", :destination => "/")
        run(cloud.dependency_resolver.compile_command)
        callback :after_configure
      end
      
      # Terminate self
      def terminate!
        cloud_provider.terminate_instance!(:instance_id => instance_id)
      end
      
      # get the os, if it's not declared
      def os(sym=nil)
        if sym
          dsl_options[:os] = sym
        else
          dsl_options[:os] ||= determine_os.to_sym
        end
      end
      alias :platform :os  # Chef uses platform, aliased for conveneince
      
      def pack_clouds_dot_rb_and_expected_directories
        %w(lib plugins).each do |dir|
          if File.directory?(d = cloud.clouds_dot_rb_dir/dir)
            dputs("Adding local path: #{d}")
            FileUtils.cp_r d, cloud.tmp_path/cloud.base_config_directory
          end
        end
        FileUtils.cp cloud.clouds_dot_rb_file, cloud.tmp_path/"/etc/poolparty/"
      end
      # Determine the os
      # Default to ubuntu
      # Send the determine_os.sh script to the node and run it remotely
      def determine_os
        scp(:source => Provision::Bootstrapper.determine_os_script, :destination => "/tmp")
        o = run("chmod +x /tmp/determine_os.sh; /bin/sh /tmp/determine_os.sh").chomp
        o.empty? ? :ubuntu : o
      end
      
      # Determine if the node is bootstrapped
      def bootstrapped?
        # @bootstrapped ||= !run('if [ -f /var/poolparty/bootstrapped ]; then echo "YES"; fi').match(/YES/).nil?
        @bootstrapped ||= !run('if [ -f /var/poolparty/bootstrapped ]; then echo "YES"; fi').chomp.empty? || false
      end
      
      # Wait for port
      # Test if the port is open and catch failures in the connection
      # Options
      #   public_ip || default public_ip
      #   retry_times || 5
      def wait_for_port(port, opts={})
        ip          = opts.delete(:public_ip)   || public_ip
        retry_times = opts.delete(:retry_times) || 10
        pause_time  = opts.delete(:pause_time)  || 1
        
        retry_times.times do |i| 
          if is_port_open?(ip, port, opts)
            return true
          else
            sleep pause_time
          end
        end
        false
      end
      
      # Wait for a public ip to be assigned, refreshing the instance data from the cloud provider on each query
      # Default timeout value of 60 seconds, can be overriden by passing {:timeout=>seconds}
      def wait_for_public_ip(timeout=60)
        ddputs("Waiting for public ip")
        begin
          Timeout::timeout(timeout) do
            loop do
              self.refresh!
              ddputs("After refreshing, the public_ip is: #{public_ip}")
              return public_ip if valid_ip?(public_ip)
              print '.'
              sleep 2
            end
          end
        rescue Timeout::Error
          return false
        end
      end
      
      def terminate!
        cloud_provider.terminate_instance!(:instance_id=>self.instance_id).first
      end
      
      # Refresh the node with fresh data from the cloud provider.
      # This is often usefully to update a recently launched instance, in case you want to trigger new behavior once the state changes ot 'running' and an ip is assigned
      # Added rescue, but not sure if this is a proper fix yet. Will need to test on live instances to ensure
      def refresh!
        dsl_options = cloud_provider.describe_instance(:instance_id => self.instance_id).dsl_options rescue {}
        self.dsl_options.merge!(dsl_options)
        self
      end
      
      ## hash like methods
      def each
        dsl_options.each{ |k,v| yield k,v }
      end
      
      def [](k)
        if dsl_options.has_key? k
          dsl_options[k]
        else
          nil
        end
      end
      
      def []=(k,v)
        dsl_options[k] = v
      end
      
      def has_key?(key)
        dsl_options.has_key?(key)
      end
      
      def keys
        dsl_options.keys
      end
         
      def values
        dsl_options.values
      end
      
      def to_hash
        dsl_options
      end
      ##end of hash like methods
      
      # Is this instance running?
      def running?
        !(status =~ /running/).nil?
      end
      # Is this instance pending?
      def pending?
        !(status =~ /pending/).nil?
      end
      # Is this instance terminating?
      def terminating?
        !(status =~ /shutting/).nil?
      end
      # Has this instance been terminated?
      def terminated?
        !(status =~ /terminated/).nil?
      end
      
      def elapsed_runtime
        Time.now.to_i - launching_time.to_time.to_i
      end
      
      # Callbacks
      def loaded
      end
            
      def on_all_callbacks(call_time, *args, &block)
        cloud.callback call_time if cloud
        super
      end
      
      def before_bootstrap
      end
      def after_bootstrap
      end
      def before_configure
      end
      def after_configure
      end
      
      # The instances is only valid if there is an internal_ip and a name
      def valid?
        (internal_ip.nil? || name.nil?) ? false : true
      end
      
      private
      
      def valid_ip?(ip)
        ip && ip != '0.0.0.0' && ip != ''
      end

      # Test for open port by opening a socket
      # on the ip and closing the socket
      def is_port_open?(ip, port, opts={})
        timeout = opts[:timeout] || 1
        begin
          Timeout::timeout(timeout) do
            begin
              s = TCPSocket.new(ip, port)
              s.close
              ddputs("Connected to #{ip}:#{port} - Port is open and good to go")
              return true
              puts ','
            rescue Errno::ECONNREFUSED, Errno::EHOSTUNREACH
              ddputs("Port #{port} on #{ip} is not accessible (yet)")
              return false
            end
          end
        rescue Timeout::Error
        end
        ddputs("Port #{port} on #{ip} is not accessible")
        return false
      end
      
    end
    
end
