class SpecException < Exception
  attr_reader :message
  
  def initialize(msg="Custom resource exception")
    @message = "Spec error: #{msg}"
  end
end