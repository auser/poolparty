module PoolParty  
  module Remote
    
    class RemoteInstance
      include Dslify
      
      dsl_methods :name,        # Name of the remote instance (internal usage)
                  :ip,          # Ip of the remote instance
                  :internal_ip, # Internal ip of the remote instance
                  :public_ip,
                  :status       # Status of the remote instance
      
      def initialize(opts={})
        set_vars_from_options(opts) if opts.is_a?(Hash)
        on_init
      end
      
      def keypair(*n)
        @keypair ||= Key.new(key_name)
      end
      
      ## hash like methods
      # TODO: move these into a module, or into dslify
      # include Enumerable
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
      
      # Callback
      def on_init
      end
      
      # The remote instances is only valid if there is an ip and a name
      def valid?
        (ip.nil? || name.nil?) ? false : true
      end
      
      # This is how we get the current load of the instance
      # The approach of this may change entirely, but the usage of
      # it will always be the same
      def load
        current_load ||= 0.0  #NOTE MF: returning 0.0 seems like a bad idea here.  should return nil if we dont have a real value
      end
      
    end
    
  end
end
