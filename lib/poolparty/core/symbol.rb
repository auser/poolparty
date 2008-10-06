class Symbol
  def to_string(prev="")
    "#{prev}#{self.to_s}"
  end
  def sanitize
    self.to_s.sanitize
  end
end