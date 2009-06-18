class Object
  
  def clouds
    $clouds ||= {}
  end
  
  def pool(name, &block)
    pools[name] ||= PoolParty::Pool.new(name, &block)
  end
  
  def pools
    $pools ||= {}
  end
  
  # Alias method
  def alias_method(new_id, original_id)
    original = self.method(original_id).to_proc
    define_method(new_id){|*args| original.call(*args)}
  end
  
  # Debugging output helpers
  def vputs(m="", o=self)
    puts "[INFO] -- #{m}" if verbose?
  end
  def dputs(m="", o=self)
    puts "[DEBUG] -- #{m.inspect}" if debugging?(o) rescue ""
  end
  def verbose?(o=self)
    o.respond_to?(:verbose) ? o.verbose : (debugging? || $TESTING ||= false)
  end
  def debugging?(o=self)
    o.respond_to?(:debug) ? o.debug : ($DEBUGGING ||= false)
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