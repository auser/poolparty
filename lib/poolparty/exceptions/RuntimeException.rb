class RuntimeException < Exception
  attr_reader :message
  
  def initialize(msg="Runtime exception")
    @message = "There was a runtime error: #{msg}"
  end
end