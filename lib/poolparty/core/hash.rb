=begin rdoc
  Hash extentions
=end
class Hash
  
  # alias_method :_old_reader, :[]
  # # Treat string and symbols the same
  # def [](key)
  #   if o = _old_reader(key) || has_key?(key)
  #     return o 
  #   elsif key.is_a? Symbol
  #     _old_reader(key.to_s)
  #   elsif key.is_a? String
  #     _old_reader(key.to_sym) rescue _old_reader(key)
  #   else
  #     _old_reader(key)
  #   end
  # end
  
  # Return a hash of all the elements where the block evaluates to true
  def choose(&block)
    Hash[*self.select(&block).inject([]){|res,(k,v)| res << k << v}]
  end

  def to_instance_variables(inst=nil)
    each do |k,v|
      inst.instance_variable_set "@#{k}", v
      inst.class.send :attr_reader, k if inst
    end
  end
  
  # extracted from activesupport
  # Returns an array of the values at the specified indices:
  #
  #   hash = HashWithIndifferentAccess.new
  #   hash[:a] = "x"
  #   hash[:b] = "y"
  #   hash.values_at("a", "b") # => ["x", "y"]
  def values_at(*indices)
    indices.collect {|key| self[key]}
  end
  
  #TODO: deprecate
  # def extract!(&block)
  #   o = Hash[*select(&block).flatten]
  #   o.keys.each {|k| self.delete(k) }
  #   o
  # end
  
  def append(other_hash)
    returning Hash.new do |h|
      h.merge!(self)
      other_hash.each do |k,v|
        h[k] = has_key?(k) ? [self[k], v].flatten.uniq : v
      end
    end
  end
  
  def append!(other_hash)
    other_hash.each do |k,v|
      self[k] = has_key?(k) ? [self[k], v].flatten.uniq : v
    end
    self
  end
  
  def safe_merge(other_hash)
    merge(other_hash.delete_if {|k,v| has_key?(k) })
  end
  
  def safe_merge!(other_hash)
    merge!(other_hash.delete_if {|k,v| has_key?(k) && !v.nil? })
  end
  
  def to_os
    m={}
    each {|k,v| m[k] = v.to_os }
    MyOpenStruct.new(m)
  end
  
  def next_sorted_key(from)
    idx = (size - keys.sort.index(from))
    keys.sort[idx - 1]
  end
  
  def stringify_keys
    dup.stringify_keys!
  end
  
  # Converts all of the keys to strings
  def stringify_keys!
    keys.each{|k| 
      v = delete(k)
      self[k.to_s] = v
      v.stringify_keys! if v.is_a?(Hash)
      v.each{|p| p.stringify_keys! if p.is_a?(Hash)} if v.is_a?(Array)
    }
    self
  end
  
  def symbolize_keys
    dup.stringify_keys!
  end
  
  # Converts all of the keys to strings
  def symbolize_keys!
    keys.each{|k| 
      v = delete(k)
      self[k.to_sym] = v
      v.symbolize_keys! if v.is_a?(Hash)
      v.each{|p| p.symbolize_keys! if p.is_a?(Hash)} if v.is_a?(Array)
    }
    self
  end
  
  
  def method_missing(sym, *args, &block)
    if has_key?(sym)
      fetch(sym)
    elsif has_key?(sym.to_s)
      fetch(sym.to_s)
    else
      super
    end
  end
end