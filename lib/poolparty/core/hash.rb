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
  
  def symbolize_keys(key_modifier=nil)
    dup.symbolize_keys!(key_modifier)
  end
  
  # Converts all of the keys to strings
  # can pass in a :key_modifier that will be sent to each key, before being symbolized.
  # This can be usefull if you want to downcase, or snake_case each key.
  # >> {'Placement' => {'AvailabilityZone'=>"us-east-1a"} }.symbolize_keys(:snake_case)
  # => {:placement=>{:availability_zone=>"us-east-1a"}}  
  def symbolize_keys!(key_modifier=nil) 
    keys.each{|k| 
      v = delete(k)
      if key_modifier && k.respond_to?(key_modifier)
        k = k.send(key_modifier)
      end
      self[k.to_sym] = v
      v.symbolize_keys!(key_modifier) if v.is_a?(Hash)
      v.each{|p| p.symbolize_keys!(key_modifier) if p.is_a?(Hash)} if v.is_a?(Array)
    }
    self
  end
  
  def to_cloud
    $:.unshift("#{::File.dirname(__FILE__)}/../../poolparty")
    require "poolparty"
    cld = Cloud.new((fetch(:name) rescue "hashed_cloud"))
    cld.set_vars_from_options(self)
    cld
  end
  
  def method_missing(sym, *args, &block)
    if has_key?(sym.to_sym)
      fetch(sym)
    elsif has_key?(sym.to_s)
      fetch(sym.to_s)
    else
      super
    end
  end  
end