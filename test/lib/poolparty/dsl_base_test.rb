require "#{File.dirname(__FILE__)}/../../test_helper"

class DslBaseTest < Test::Unit::TestCase
  context "DslBase" do
    setup do
      @base = PoolParty::DslBase.new
    end
    
    should "have the method instances" do
      @base.instances 1
      assert_equal 1, @base.minimum_instances
      assert_equal 1, @base.maximum_instances      
      @base.instances 3..20
      assert_equal 3, @base.minimum_instances
      assert_equal 20, @base.maximum_instances
      
      PoolParty::PoolPartyError.create("DslMethodCall") # To make sure the constant is defined below
      assert_raises DslMethodCall do
        @base.instances "box"
      end
    end
    
  end  
  
end