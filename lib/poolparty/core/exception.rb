=begin rdoc
  Exception overloads
=end
class Exception
  # Gives us a nice_message for exceptions
  def nice_message(padding="")
    "#{padding}#{message}\n#{padding}" + backtrace.join("\n#{padding}")
  end
end