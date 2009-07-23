class Object
    
  # The pool method creates a pool and
  # inserts it into the pool hash
  def pool(name, &block)
    if block
      pools[name.to_s] ||= PoolParty::Pool.new(name, &block)
    else
      raise PoolParty::PoolPartyError.create("PoolError", "You must pass a block when defining a pool")
    end
  end
  
  # The global hash of pools
  def pools
    $pools ||= {}
  end
  
  # The clouds hash is a global hash
  # that all objects can retrieve clouds from
  # and they are stored by name
  def clouds
    $clouds ||= {}
  end
  
  # Alias method
  def alias_method(new_id, original_id)
    original = self.method(original_id).to_proc
    define_method(new_id){|*args| original.call(*args)}
  end
  
  # Benchmark a block
  def bm(msg, &block)
    t = Time.now
    block.call
    puts "#{msg}: #{Time.now-t}"
  end
  # Debugging output helpers
  def vputs(m="", o=self)
    puts "[INFO] -- #{m}" if verbose?(o)
  end
  def dputs(m="", o=self)
    puts "[DEBUG] -- #{m.inspect}" if debugging?(o)
  end
  def ddputs(m="", o=self)
    puts "[DEBUG] -- #{m.inspect}" if very_debugging?(o)
  end
  def verbose?(o=self)
    o.respond_to?(:verbose) ? o.verbose : (debugging? || $TESTING || false)
  end
  def debugging?(o=self)
    o.respond_to?(:debug) ? o.debug : ($DEBUGGING ||= false)
  end
  def very_debugging?(o=self)
    o.respond_to?(:very_debugging) ? o.very_debugging : ($VERY_DEBUGGING ||= false)
  end
  # Do once.
  # Takes a block. IF this block has already been run (from the run_procs array),
  # then run it and store the block unique id in the run_procs array so
  # it never gets run again
  def do_once(&block)
    unless run_procs.include?(block.to_s)      
      instance_eval &block if block
      run_procs << block.to_s
    end    
  end
  
  # Procs that have been run already in the run_once blocks
  # This is just a container array of procs
  def run_procs
    @run_procs ||= []
  end
  
end