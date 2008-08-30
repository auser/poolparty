class ResourceException < Exception
  attr_reader :message
  
  def initialize(msg="Custom resource exception")
    @message = "Custom resource error: #{msg}"
  end
end