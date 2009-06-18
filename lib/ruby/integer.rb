class Integer
  def months
    self * 30.days
  end
  alias :month :months

  def years
    self * 365.25.days
  end
  alias :year :years
end
