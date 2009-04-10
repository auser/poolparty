=begin rdoc
  Hash extentions
=end
class Hash
  def choose(&block)
    Hash[*self.select(&block).inject([]){|res,(k,v)| res << k << v}]    
  end

  def to_instance_variables(inst=nil)
    each do |k,v|
      inst.instance_variable_set "@#{k}", v
      inst.class.send :attr_reader, k if inst
    end
  end
  
  def key_strings_to_symbols!
    r = Hash.new
    self.each_pair do |k,v|
      if (k.kind_of? String)
        v.key_strings_to_symbols! if v.kind_of? Hash and v.respond_to? :key_strings_to_symbols!
        r[k.to_sym] = v
      else
        v.key_strings_to_symbols! if v.kind_of? Hash and v.respond_to? :key_strings_to_symbols!
        r[k] = v
      end
    end
    self.replace(r)
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
  def method_missing(sym, *args, &block)
    has_key?(sym) ? fetch(sym) : super
  end
  def next_sorted_key(from)
    idx = (size - keys.sort.index(from))
    keys.sort[idx - 1]
  end
end