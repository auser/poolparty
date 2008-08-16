=begin rdoc
  Hash extentions
=end
class Hash
  def safe_merge(other_hash)
    merge(other_hash.delete_if {|k,v| has_key?(k) })
  end
  def safe_merge!(other_hash)
    merge!(other_hash.delete_if {|k,v| has_key?(k) && !v.nil? })
  end
  def flush_out_options(opts={})
    map {|k,v| "#{opts[:prev]}#{k} => #{v}#{opts[:post]}" }
  end
  def to_os
    m={}
    each {|k,v| m[k] = v.to_os }
    MyOpenStruct.new(m)
  end
end