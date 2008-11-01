require File.dirname(__FILE__) + "/remoter"

module PoolParty  
  module Remote
    
    class RemoteInstance
      include Remote
      include Configurable
      include CloudResourcer
            
      def initialize(opts, parent=self)
        @parent = parent
        set_vars_from_options(parent.options) if parent && parent.respond_to?(:options)
        set_vars_from_options(opts) unless opts.nil? || opts.empty?
        on_init
      end
      
      # Callback
      def on_init        
      end
      
      # Is this remote instance the master?
      def master?
        name == "master"
      end
      
      # The remote instances is only valid if there is an ip and a name
      def valid?
        !(ip.nil? || name.nil?)
      end
      
      # Determine if the RemoteInstance is responding
      def responding?
        !responding.nil?
      end
      
      # This is how we get the current load of the instance
      # The approach of this may change entirely, but the usage of
      # it will always be the same
      def load
        current_load ||= 0.0
      end
            
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
      
      # Printing. This is how we extract the instances into the listing on the 
      # local side into the local listing file
      def to_s
        "#{name}\t#{ip}"
      end
      
      def puppet_runner_command
        self.class.send :puppet_runner_command
      end
      # Commands for the servers
      def self.puppet_runner_command
        ". /etc/profile && /usr/sbin/puppetd --onetime --no-daemonize --logdest syslog --server master 2>&1"
      end
      def self.puppet_rerun_commad
        "/usr/bin/puppetrerun 2>&1 > /dev/null"
      end
    end
    
  end  
end