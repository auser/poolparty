class TemplateNotFound < Exception
  attr_reader :message
  
  def initialize(msg="Custom resource exception")
    @message = "Template not found: #{msg}"
  end
end