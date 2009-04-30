class MasterException < Exception
  @messages = {
    :no_ip => "Master does not have an ip or has not been launched"
  }
  attr_reader :message
  
  def initialize(type=:no_ip, note="")
    @message = "Master Exception: #{@messages[type]} #{note}"
  end
end