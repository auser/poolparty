=begin rdoc
  Hash extentions
=end
class Hash
    
  # Return a hash of all the elements where the block evaluates to true
  def choose(&block)
    Hash[*self.select(&block).inject([]){|res,(k,v)| res << k << v}]
  end
  
  # Computes the difference between two hashes
  def diff(other, *hsh)
    o = {}
    keys.map do |k|
      if hsh.include?(k) || hsh.empty?
        other[k] == self[k] ? nil : o.merge!({k => other[k]})
      end
    end.reject {|b| b.nil? }
    o
  end
  
  def merge_if!(k, v)
    self[k] = v if v
    self
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