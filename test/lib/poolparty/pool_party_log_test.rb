require "#{File.dirname(__FILE__)}/../../test_helper"

class PPLogTest < Test::Unit::TestCase
  context "Logger" do
        
    should "have a logger" do
      str = capture_stdout do
        PoolParty::PoolPartyLog.info "hi"
      end
      assert_match /hi/, str
      assert_match /INFO/, str
    end
  end
  
  
  
end