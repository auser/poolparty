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
end