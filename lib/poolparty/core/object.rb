=begin rdoc
  Basic, add an alias_method to the object class
  Add returning to the object
=end
class Object
  def my_methods
    self.methods.sort - (self.class.methods + self.class.superclass.methods)
  end
  def this
    self
  end
  def to_os
    self
  end  
  def alias_method(new_id, original_id)
    original = self.method(original_id).to_proc
    define_method(new_id){|*args| original.call(*args)}
  end

  def returning(receiver)
    yield receiver
    receiver
  end
  def extended(&block)
    block.in_context(self).call
    self
  end
  
  # Procs that have been run already in the run_once blocks
  # This is just a container array of procs
  def run_procs
    @run_procs ||= []
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

  def respec_string
    case self.class
    when String
      self.to_option_string
    when Array
      self.map {|a| "#{a.respec_string}" }.join(" ")
    else
      "'#{self}'"
    end
  end
  def block_instance_eval(*args, &block)
    return instance_eval(*args,&block) unless block && !block.arity.zero?
    old_method = (self.class.instance_method(:__) rescue nil)
    self.class.send(:define_method, :__, &block)
    block_method = self.class.instance_method(:__)
    if old_method
      self.class.send(:define_method, :__, old_method)
    else
      self.class.send(:remove_method, :__)
    end
    block_method.bind(self).call(*args)
  end
  def meta_def name, &blk
    meta_eval { define_method name, &blk }
  end
  def meta_undef name
    meta_eval { remove_method name }
  end
  # def run_in_context(context=self, &block)
  #   name="temp_#{self.class}_#{respond_to?(:parent) ? parent.to_s : Time.now.to_i}".to_sym
  #   meta_def name, &block
  #   self.send name, context
  #   meta_undef name rescue ""
  # end
  def vputs(m="", o=self)
    # puts m if o.verbose rescue ""
    puts "[INFO] -- #{m}" if verbose?
  end
  def vprint(m="", o=self)
    print m if o.verbose rescue ""
  end
  def dputs(m="", o=self)
    puts "[DEBUG] -- #{m.inspect}" if debugging?(o) rescue ""
  end
  def dprint(m="", o=self)
    print "#{m}" if debugging?(o) || verbose?(o) rescue ""
  end
  def verbose?(o=self)
    o.respond_to?(:verbose) ? o.verbose : (debugging? || $TESTING ||= false)
  end
  def debugging?(o=self)
    o.respond_to?(:debug) ? o.debug : ($DEBUGGING ||= false)
  end
  def debugging(bool=nil)
    bool.nil? ? $DEBUGGING : $DEBUGGING = bool
  end
  def testing(bool=$TESTING)
    bool.nil? ? $TESTING : $TESTING = bool
  end
  alias :debug :debugging
  def unix_hide_string
    "2>&1 > /dev/null"
  end

  # Get object's meta (ghost, eigenclass, singleton) class
  # from activesupport
  def metaclass
    class << self
      self
    end
  end

  # If class_eval is called on an object, add those methods to its metaclass
  # from activesupport
  def class_eval(*args, &block)
    metaclass.class_eval(*args, &block)
  end
end
