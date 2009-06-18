=begin rdoc
  Base class for all PoolParty objects
=end
module PoolParty
  
  # Global storage for the current context_stack
  def context_stack
    $context_stack ||= []
  end
  
  class Base
    attr_reader :init_opts
    include Parenting, Dslify
    include SearchablePaths
    include Callbacks
    
    # Set the searchable paths to look in the default locations
    has_searchable_paths
    
    # So methods inside the default_options do not have to 
    # have default options and can pull from their parents
    # but the option is still pulled for the printed default_options
    def self.additional_options(*o)
      dsl_options.merge!(o.inject({}) {|s,i| s.merge(i => nil)})
    end
    
    def initialize(opts={}, extra_opts={}, &block)      
      @init_block = block
      @init_opts = compile_opts(opts, extra_opts)
      @base_name = @init_opts[:name]

      # run_in_context(init_opts, &block)
      run_with_callbacks(init_opts, &block)
    end
    
    # Overloading the parent run_in_context
    # First, push ourself to the stack
    # then set all the variables given with the init
    # and then eval the block and pop ourselves off the stack
    # During runtime, the stack looks like
    # 
    # - self
    #   - instance_eval block
    # - stack
    def run_in_context(o={}, &block)      
      context_stack.push self        
      set_vars_from_options(o)
      instance_eval &block if block
      context_stack.pop
    end
    
    # Run the block in the context of self
    # and call the block after calling 
    #   before_load
    # and afterwards calling
    #   after_loaded
    def run_with_callbacks(o, &block)
      run_in_context(o) do
        before_load(o, &block)
        yield if block_given?
        after_loaded(o, &block)
      end
    end
    
    # Base callbacks for run_with_callbacks
    # before_load
    # This is called before the block is yielded
    def before_load(o={}, &block)      
    end
    
    # after_loaded
    # This is run immediately after the block given
    # on init is yielded
    def after_loaded(o={}, &block)
    end
    
    # Try to extract the name from the options
    # Either the first parameter is a string or it is a hash
    # if it is a hash, just merge the two hashes
    # If it is a string, then merge it in as the name of the
    # instance on the hash
    def compile_opts(o={}, extra={})
      case o
      when String
        extra.merge(:name => o)
      else
        extra.merge(o)
      end
    end
    
    # Ordered resources
    # are the resources associated with this base
    def ordered_resources
      @ordered_resources ||= []
    end    
    
    # If the method is missing from ourself, check the Default
    # class for the corresponding method.
    # If the Default class has the method, inquire from the Default
    # class, otherwise, call super
    def method_missing(m,*a,&block)
      if Default.respond_to?(m)
        Default.send(m,*a,&block)
      else
        super
      end
    end
    
  end
end