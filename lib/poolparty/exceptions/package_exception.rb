class PackageException < Exception
  attr_reader :message
  
  def initialize(msg="Package error")
    @message = "Package error: #{msg}"
  end
end