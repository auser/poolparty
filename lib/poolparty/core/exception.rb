=begin rdoc
  Exception overloads
=end
class Exception
  alias :nice_message :to_s
  # Gives us a nice_message for exceptions
  def nice_message(padding="")
    "#{padding}#{message}\n#{padding}"# + backtrace.join("\n#{padding}")
  end
end