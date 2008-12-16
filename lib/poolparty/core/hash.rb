=begin rdoc
  Hash extentions
=end
class Hash
  alias :old_select :select

  def select(&block)
    Hash[*self.old_select(&block).inject([]){|res,(k,v)| res << k << v}]    
  end
  def extract!(&block)
    o = select(&block)
    o.keys.each {|k| self.delete(k) }
    o
  end
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
  def flush_out(pre="", post="")
    map {|k,v| "#{pre}#{k} => #{v.to_option_string}#{post}"}
  end
  def to_os
    m={}
    each {|k,v| m[k] = v.to_os }
    MyOpenStruct.new(m)
  end
  def method_missing(sym, *args, &block)
    key?(sym) ? fetch(sym) : super
  end
end