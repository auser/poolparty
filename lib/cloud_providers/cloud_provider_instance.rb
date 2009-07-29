require 'socket'
require 'timeout'

module CloudProviders
  
  class CloudProviderInstance
      include Dslify
      # include Enumerable
      include Connections
      
      default_options(
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
      
      # Bootstrap self
      def bootstrap!(force=false)
        old_user = user
        @user = "root"
        
        if !bootstrapped? || force
          script_file = Provision::Bootstrapper.bootstrap_script(os)
          scp(:source => script_file, :destination => "/tmp")
          run("chmod +x /tmp/determine_os.sh; /bin/sh /tmp/#{File.basename(script_file)}").chomp
        end
        
        @user = old_user
      end
      
      # Configure the node
      def configure!(opts={})
        raise StandardError.new("You must pass in a cloud to configure an instance") unless opts.has_key?(:cloud)
        cld = opts[:cloud]
        cld.compile(self)
        script_file = Provision::Bootstrapper.configure_script(cld, os)
        
        FileUtils.mkdir_p cld.tmp_path/"etc"/"poolparty" unless File.directory?(cld.tmp_path/"etc"/"poolparty")
        FileUtils.cp script_file, cld.tmp_path/"etc"/"poolparty"
        
        puts rsync(:source => cld.tmp_path/"*", :destination => "/")
        puts run("chmod +x /etc/poolparty/#{File.basename(script_file)}; /bin/sh /etc/poolparty/#{File.basename(script_file)}").chomp
        puts run("/usr/bin/chef-solo -c /etc/chef/solo.rb -j /etc/chef/dna.json")
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
      alias :platform :os
      
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
        @bootstrapped ||= !run('if [ -f /var/poolparty/bootstrapped ]; then echo "YES"; fi').chomp.empty? || false
      end
      
      
      # Wait for port
      # Test if the port is open and catch failures in the connection
      # Options
      #   public_ip || default public_ip
      #   retry_times || 5
      def wait_for_port(port, opts={})
        ip          = opts.delete(:public_ip) || public_ip
        retry_times = opts.delete(:retry_times) || 5
        
        retry_times.times {|i| return is_port_open?(ip, port, opts)}
        false
      end
      
      # Wait for a public ip to be assigned, refreshing the instance data from the cloud provider on each query
      # Default timeout value of 60 seconds, can be overriden by passing {:timeout=>seconds}
      def wait_for_public_ip(timeout=60)
        begin
          Timeout::timeout(timeout) do
            loop do
              refresh!
              return public_ip if public_ip and public_ip != '0.0.0.0'
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
      def refresh!
        refreshed = cloud_provider.describe_instance(:instance_id => self.instance_id)
        self.dsl_options.merge!(refreshed.dsl_options)
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
      
      def before_bootstrap
      end
      def after_bootstrap
      end
      def before_configure
      end
      def after_configure
      end
      
      def cloud(n=nil)
        @cloud ||= n
      end
      
      # The instances is only valid if there is an internal_ip and a name
      def valid?
        (internal_ip.nil? || name.nil?) ? false : true
      end
      
      private
      
      # Test for open port by opening a socket
      # on the ip and closing the socket
      def is_port_open?(ip, port, opts={})
        timeout = opts[:timeout] || 1
        begin
          Timeout::timeout(timeout) do
            begin
              s = TCPSocket.new(ip, port)
              s.close
              return true
            rescue Errno::ECONNREFUSED, Errno::EHOSTUNREACH
              return false
            end
          end
        rescue Timeout::Error
        end
        return false
      end
      
    end
    
end
