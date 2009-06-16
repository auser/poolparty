require "#{File.dirname(__FILE__)}/../../test_helper"

class DefaultTest < Test::Unit::TestCase
  context "Default" do
    should "have default users" do
      p PoolParty::Default.dsl_options
      assert_equal PoolParty::Default.user, "root"
    end
  end
  
end