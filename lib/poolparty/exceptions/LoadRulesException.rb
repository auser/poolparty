class LoadRulesException < Exception
  attr_reader :message
  
  def initialize(msg="It looks like your rules are malformed")
    @message = "Load Malformed rule: #{msg}"
  end
end