class RemoteException < Exception
  EXCEPTION_MESSAGES = {
    :method_not_defined => "Method is not defined",
    :invalid_formatting => "Invalid formatting",
    :could_not_install => "Could not install"
  } unless const_defined?("EXCEPTION_MESSAGES")
  attr_reader :message
  
  def initialize(type=:method_not_defined, note="")
    @message = "Remote Exception: #{EXCEPTION_MESSAGES[type]} #{note}"
  end
end