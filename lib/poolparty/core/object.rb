=begin rdoc
  Basic, add an alias_method to the object class
  Add returning to the object
=end
class Object
  def my_methods
    self.methods.sort - (self.class.methods + self.class.superclass.methods)
  end
  def to_os
    self
  end  
  def alias_method(new_id, original_id)
    original = self.method(original_id).to_proc
    define_method(new_id){|*args| original.call(*args)}
  end
  def with_options(opts={}, parent=self, &block)
    @p = parent.clone
    @p.options.merge!(opts)
    @p.instance_eval &block if block
  end  
  def returning(receiver)
    yield receiver
    receiver
  end
  def extended(&block)
    block.in_context(self).call
    self
  end
  def send_if_method(v, *args)
    if (v.nil? || v.to_s.empty? || v.is_a?(Array) || v.is_a?(Integer))
      v
    else
      vs = v.to_s.to_sym
      respond_to?(vs) ? self.send(vs, *args) : v
    end
  end
  def to_option_string
    case self.class
    when String
      self.to_option_string
    when Array
      self.each {|a| a.to_option_string }.join(" ")
    else
      "#{self}"
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
    return instance_eval(*args,&block) unless block_given? && !block.arity.zero?
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
  def run_in_context(&block)
    name="temp_#{self.class}_#{respond_to?(:parent) ? parent.to_s : Time.now.to_i}".to_sym
    meta_def name, &block
    self.send name, self
    # self.instance_eval &block if block
    meta_undef name
  end
  def vputs(m="", o=self)
    puts m if o.verbose
  end
  def vprint(m="", o=self)
    print m if o.verbose
  end
  def unix_hide_string
    "2>&1 > /dev/null"
  end
end