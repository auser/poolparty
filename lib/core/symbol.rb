class Symbol
  # def >(num);"#{self} > #{num}";end
  # def <(num);"#{self} < #{num}";end
  # def >=(num);"#{self} >= #{num}";end
  # def <=(num);"#{self} <= #{num}";end
  # def ==(num);"#{self} > #{num}";end
  
  def sanitize
    self.to_s.sanitize
  end
  def <=>(b)
    "#{self}" <=> "#{b}"
  end
  ##
  # @param o<String, Symbol> The path component to join with the string.
  #
  # @return <String> The original path concatenated with o.
  #
  # @example
  #   :merb/"core_ext" #=> "merb/core_ext"
  def /(o)
    File.join(self.to_s, o.to_s)
  end
  
  # Classify the symbol
  def classify
    to_s.classify.to_sym
  end
end