require "#{File.dirname(__FILE__)}/../../test_helper"

include_fixture_resources

class ResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @inst = FakeResource.new
    end
    
    should "have the method denoted by has_method_name" do
      assert_equal "tester", FakeResource.has_method_name
      assert @inst.respond_to?(:has_tester)
      assert_equal FakeResource, @inst.has_tester.class
      assert_equal FakeResource, @inst.tester.class
      assert_equal FakeResource, @inst.does_not_have_tester.class
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
    
    should "have be able to pull out a resource with get_resource" do
      assert @inst.respond_to?(:get_tester)
      res = @inst.has_tester "hi"
      assert_equal true, @inst.testers[0].exists?
      assert_equal res, @inst.get_tester("hi")
    end
  end
  
  context "print_to methods" do
    setup do
      @inst = FakeResource.new
    end

    should "have the method print_to_chef" do
      assert @inst.respond_to?(:print_to_chef)
      assert !@inst.respond_to?(:print_to_non_existant_dependency_resolver)
    end
  end
  
  context "helpers" do
    setup do
      @inst = FakeResource.new
    end
    should "print_variables" do
      assert_equal @inst.print_variable("a"), "\"a\""
      assert_equal @inst.print_variable(:a), ":a"
      assert_equal @inst.print_variable({:a => "a"}), ":a => \"a\""
      assert_equal @inst.print_variable(644), "644"
      assert_equal @inst.print_variable("0755"), "0755"
      assert_equal @inst.print_variable("755"), "0755"
      assert_equal @inst.print_variable(@inst), @inst.to_s
      assert_equal @inst.print_variable(nil), nil
    end
  end
  
  
end