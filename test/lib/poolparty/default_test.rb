require "#{File.dirname(__FILE__)}/../../test_helper"

class DefaultTest < Test::Unit::TestCase
  context "Default" do
    should "have default users (in the default_options)" do
      assert_equal PoolParty::Default.user, "root"
    end
  end
  
end