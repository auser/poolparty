# A Poor mans Priority Queue. (Very inefficent but minimal implemention).
class PoorPriorityQueue < Hash
  def push(object, priority)
    self[object] = priority
  end

  def min
    return nil if self.empty?
    min_k = self.keys.first
    min_p = self[min_k]
    self.each do | k, p |
      min_k, min_p = k, p if p < min_p
    end
    [min_k, min_p]
  end

  def min_key
    min[0] rescue nil
  end
  
  def min_priority
    min[1] rescue nil
  end

  def delete_min
    return nil if self.empty?
    min_k, min_p = *min
    self.delete(min_k)
    [min_k, min_p]
  end

  def delete_min_return_key
    delete_min[0] rescue nil
  end
  
  def delete_min_return_priority
    delete_min[1] rescue nil
  end

  def delete(object)
    return nil unless self.has_key?(object) 
    result = [object, self[object]]
    super
    result
  end
end
