require File.dirname(__FILE__) + "/remoter"

module PoolParty  
  module Remote
    
    class RemoteInstance
      include Remote
      
      attr_reader :ip, :name
      
      def initialize(opts)
        case opts.class.to_s.downcase
        when "hash"
          @ip = opts[:ip]
          @name = opts[:name]
          @load = opts[:load]
          @responding = opts[:responding]
          @status = opts[:status]
        when "string"
          @name, @ip, @status, @responding, @load = opts.split(" ")
        end
        
        on_init
      end
      
      # Callback
      def on_init        
      end
      
      # Is this remote instance the master?
      def master?
        @name == "master"
      end
      
      # The remote instances is only valid if there is an ip and a name
      def valid?
        !(@ip.nil? || @name.nil?)
      end
      
      # Determine if the RemoteInstance is responding
      def responding?
        @is_responding ||= @responding
      end
      
      # This is how we get the current load of the instance
      # The approach of this may change entirely, but the usage of
      # it will always be the same
      def load
        @current_load ||= @load
      end
      
      # Get the status of the instance
      def status
        @status ||= "running"
      end
      
      # Is this instance running?
      def running?
        status =~ /running/
      end
      # Is this instance pending?
      def pending?
        status =~ /pending/
      end
      # Is this instance terminating?
      def terminating?
        status =~ /shutting/
      end
      # Has this instance been terminated?
      def terminated?
        status =~ /terminated/
      end
      
      # Printing. This is how we extract the instances into the listing on the 
      # local side into the local listing file
      def to_s
        "#{@name} #{@ip} #{responding?} #{load}"
      end
    end
    
  end  
end