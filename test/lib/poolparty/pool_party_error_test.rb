require "#{File.dirname(__FILE__)}/../../test_helper"

class PoolPartyErrorTest < Test::Unit::TestCase
  include PoolParty
  context "Error" do
    should "be able to create a PoolPartyError" do
      assert_nothing_raised do
        PoolPartyError.new "TestError", "New error"
      end
    end
    
    should "raise when called with the test error" do
      assert_raise TestError do
        raise PoolPartyError.new("TestError", "New error")
      end
    end
  end
  
end