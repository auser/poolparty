module PoolParty  
  module Remote
    
    class RemoteInstance
      include Dslify
      
      default_options(
        :name => nil,
        :ip => nil,
        :internal_ip => nil,
        :status => nil
      )
      
      def initialize(opts={}, containing_cloud=nil)
        @parent = containing_cloud

        set_vars_from_options(containing_cloud.options) if containing_cloud && containing_cloud.respond_to?(:options)
        set_vars_from_options(opts) if opts.is_a?(Hash)
        on_init
      end
      
      def elapsed_runtime
        Time.now.to_i - launching_time.to_time.to_i
      end
      
      # Callback
      def on_init
      end
      
      # The remote instances is only valid if there is an ip and a name
      def valid?
        (ip.nil? || name.nil?) ? false : true
      end
      
      # Determine if the RemoteInstance is responding
      def responding?
        running?
        # !responding.nil? #TODO MF this needs to actually ping the node or something similar.  stubbed to running? for now
      end
      
      # This is how we get the current load of the instance
      # The approach of this may change entirely, but the usage of
      # it will always be the same
      def load
        current_load ||= 0.0  #NOTE MF: returning 0.0 seems like a bad idea here.  should return nil if we dont have a real value
      end
      
      # Note, the next 4 methods will be overridden by the cloud specific remoter_base
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
      
      # Printing. This is how we extract the instances into the listing on the 
      # local side into the local listing file
      def to_s
        "#{name}\t#{ip}\t#{instance_id rescue ""}"
      end
      
      # Class method to disect a neighborhood line
      def self.hash_from_s(s)
        arr = s.split("\t")
        {:name => arr[0], :ip => arr[1]}
      end
      
      def self.to_s(hsh)
        new(hsh).to_s
      end
      
      def hosts_file_listing_for(cl)
        string = (cl.name == cloud.name) ? "#{name}.#{my_cloud.name}\t#{name}" : "#{name}.#{my_cloud.name}"
        "#{internal_ip}\t#{string}"
      end
    end
    
  end  
end
