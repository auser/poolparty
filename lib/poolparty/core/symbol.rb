class Symbol
  # def >(num);"#{self} > #{num}";end
  # def <(num);"#{self} < #{num}";end
  # def >=(num);"#{self} >= #{num}";end
  # def <=(num);"#{self} <= #{num}";end
  # def ==(num);"#{self} > #{num}";end
  
  def to_string(pre="")
    "#{pre}#{self.to_s}"
  end
  def sanitize
    self.to_s.sanitize
  end
  def <=>(b)
    "#{self}" <=> "#{b}"
  end
end