class ProvisionerException < Exception  
  def initialize(msg="Provisioner process failed")
    @message = "Error: #{msg}"
  end
end