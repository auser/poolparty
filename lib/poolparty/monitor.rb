=begin rdoc
  Monitor
  
  This provides the interface to monitors in a clouds.rb configuration file
  
  == Usage
    PoolParty::Monitor.new(:cpu) do |c|
      vote_for(:expand) if c > 0.8
      configure if c < 0.1
    end
  
  The methods inside the block will be executed on the cloud with the value
  retrieved by the monitor. 
=end
module PoolParty
  class Monitor
    
    attr_reader :name, :monitor_block, :value_format
    
    def initialize(monitor_name, &block)
      msg =<<-EOE
You must pass a block with your monitor
  Example:
    monitor :cpu do |c|
      vote_for(:expand) if c > 0.8
      configure if c < 0.1
    end
      EOE
      raise PoolPartyError.create("MonitorDefinitionError", msg) unless block
      @name = monitor_name.to_sym
      @monitor_block = block
    end
    
    # Run the block given with the monitor
    # with the given values on run. Clear the methods
    # hash out so they don't conflict with the previous values
    # retrieved and return the methods available.
    def run(val)
      @methods = nil
      instance_exec format_value(val), &monitor_block
      methods
    end
    
    # Format the monitor values
    # Set the monitor format here.
    # The default will be to turn the value into a float
    # but to allow other formats, call the value here, for instance:
    #   mon.format :to_s
    # Blocks are also permitted
    def format(meth=nil, &block)
      @value_format ||= (meth ? meth : block)
    end
    
    private
    
    # Format the value of the monitor
    def format_value(value)
      case value_format
      when Proc
        value_format.call(value)
      when nil
        value.to_f
      else
        value.send value_format
      end
    end

    # We don't want the methods actually executing since we are executing the methods 
    # in a cloud, we just want to store the output values of the
    # methods, so we'll store them in a hash with the method as the key
    # and the value of the method as the value in an array
    def method_missing(meth,*a,&block)
      (methods[meth] ||= []) << a
      methods[meth].flatten!
    end
    
    # Local storage for the methods
    def methods
      @methods ||= {}
    end
    
  end
end