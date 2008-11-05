class CloudNotFoundException < Exception
  attr_reader :message
  
  def initialize(msg="Cloud not found")
    @message = "Cloud not found: #{msg}"
  end
end