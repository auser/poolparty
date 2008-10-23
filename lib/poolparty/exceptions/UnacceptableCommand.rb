class UnacceptableCommand < Exception  
  def initialize(msg="Unacceptable command")
    @message = "Disallowed or unacceptable command error: #{msg}"
  end
end