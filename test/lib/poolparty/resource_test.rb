require "#{File.dirname(__FILE__)}/../../test_helper"

class ResourceTestClass < PoolParty::Resource
  def self.has_method_name
    "tester"
  end
end

class ResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @inst = ResourceTestClass.new
    end
    
    should "have the method denoted by has_method_name" do
      assert_equal "tester", ResourceTestClass.has_method_name
      assert @inst.respond_to?(:has_tester)
      assert_equal ResourceTestClass, @inst.has_tester.class
      assert_equal ResourceTestClass, @inst.tester.class
      assert_equal ResourceTestClass, @inst.does_not_have_tester.class
      assert_equal @inst.testers.size, 3
    end
    
    should "denote the has_ methods appropriately" do
      @inst.has_tester
      assert_equal true, @inst.testers[0].exists?
      @inst.tester
      assert_equal true, @inst.testers[1].exists?
      @inst.does_not_have_tester
      assert_equal false, @inst.testers[2].exists?
    end
  end
  
  context "print_to methods" do
    setup do
      @inst = ResourceTestClass.new
    end

    should "have the method print_to_chef" do
      assert @inst.respond_to?(:print_to_chef)
      assert !@inst.respond_to?(:print_to_non_existant_dependency_resolver)
    end
  end
  
  
end