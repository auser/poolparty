=begin rdoc
  Hash extentions
=end
class Hash
  alias :old_select :select

  def select(&block)
    Hash[*self.old_select(&block).inject([]){|res,(k,v)| res << k << v}]    
  end
  def extract!
    Hash[*self.old_select { |k, v| yield(k, v) && self.delete(k) }.flatten]
  end
  def safe_merge(other_hash)
    merge(other_hash.delete_if {|k,v| has_key?(k) })
  end
  def safe_merge!(other_hash)
    merge!(other_hash.delete_if {|k,v| has_key?(k) && !v.nil? })
  end
  def flush_out(prev="", post="")
    map {|k,v| "#{prev}#{k} => #{v.to_option_string}#{post}"}
  end
  def to_os
    m={}
    each {|k,v| m[k] = v.to_os }
    MyOpenStruct.new(m)
  end
end