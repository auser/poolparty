class Integer
  def months
    PoolParty::Duration.new(self * 30.days, [[:months, self]])
  end
  alias :month :months

  def years
    PoolParty::Duration.new(self * 365.25.days, [[:years, self]])
  end
  alias :year :years
end
