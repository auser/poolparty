=begin rdoc
  Based off the rails Numeric class.
  Gives us the ability to use nice phrases such as
  30.seconds, 5.days, etc.
=end
class Numeric
  def ago(time = Time.now)
    time - self
  end
  alias :until :ago

  def since(time = Time.now)
    time + self
  end
  alias :from_now :since
  
  def seconds
    self
  end
  alias :second :seconds

  def minutes
    self * 60
  end
  alias :minute :minutes  

  def hours
    self * 60.minutes
  end
  alias :hour :hours

  def days
    self * 24.hours
  end
  alias :day :days

  def weeks
    self * 7.days
  end
  alias :week :weeks
  
  def months
    self * 31.days
  end
  alias :month :months
  
  def time_ago
    out = %w(year month week day hour minute).detect {|unit| self > 1.send(unit) }
    units_ago(out, self) rescue "Less than a minute ago"
  end

  def units_ago(unit,seconds)
    in_units = (seconds / 1.send(unit))
    "#{in_units.to_i} #{in_units != 1 ? unit.to_s.pluralize : unit} ago" 
  end
end