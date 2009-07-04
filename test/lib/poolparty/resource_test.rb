require "#{File.dirname(__FILE__)}/../../test_helper"

class ResourceTestClass < PoolParty::Resource
  def has_method_name
    "tester"
  end
end

class ResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      @inst = ResourceTestClass.new
    end
    
    should "have the method denoted by has_method_name" do
      assert_equal "tester", @inst.has_method_name
      assert @inst.respond_to?(:has_tester)
      @inst.has_tester
    end
  end
  
  
  
end