module PoolParty
  class Pool < Base
    
    # Freeze the pool_name so we can't modify it at all
    # call and run instance_eval on the block and then call the after_create callback
    def initialize(n, &block)
      context_stack.clear
      
      @pool_name = n
      @pool_name.freeze
      
      callback :before_create
      super(&block)
      callback :after_create
    end
    
    # cloud
    # Define a cloud by a name and a block
    def cloud(name, &block)
      if block
        clouds[name] ||= PoolParty::Cloud.new(name, &block)
      else
        raise PoolPartyError.new("CloudError", "You must pass a block when defining a cloud")
      end    
    end
    
  end
end