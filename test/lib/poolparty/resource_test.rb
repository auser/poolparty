require "#{File.dirname(__FILE__)}/../../test_helper"

include_fixture_resources

class ResourceTest < Test::Unit::TestCase
  def setup
    PoolParty::Resource.define_resource_methods
    @inst = FakeResource.new
  end
    
  def test_have_the_method_denoted_by_has_method_name
    assert_equal "tester", FakeResource.has_method_name
    assert @inst.respond_to?(:has_tester)
    assert_equal FakeResource, @inst.has_tester.class
    assert_equal FakeResource, @inst.tester.class
    assert_equal FakeResource, @inst.does_not_have_tester.class
    assert_equal @inst.testers.size, 3
  end
  
  def test_denote_the_has_methods_appropriately
    @inst.has_tester
    assert_equal true, @inst.testers[0].exists?
    @inst.tester
    assert_equal true, @inst.testers[1].exists?
    @inst.does_not_have_tester
    assert_equal false, @inst.testers[2].exists?
  end
  
  def test_have_be_able_to_pull_out_a_resource_with_get_resource
    assert @inst.respond_to?(:get_tester)
    res = @inst.has_tester "hi"
    assert_equal true, @inst.testers[0].exists?
    assert_equal res, @inst.get_tester("hi")
  end
  
  def test_be_able_to_pull_out_a_resource_nested_in_another_resource
    @inst.has_tester "parent" do
      has_tester "phony"
      has_tester "real" do
        self.class.send :attr_reader, :apples
        @apples = get_tester("phony")
      end
    end
    
    parent = @inst.testers.first
    assert_equal "parent", parent.name
    assert_equal "phony", parent.testers.first.name
    assert_equal "real", parent.testers[1].name
    assert_equal parent.testers.first, parent.testers[1].apples
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
  
  def test_defined_resources
    FakeResource.method_defined!
    assert_equal true, FakeResource.method_defined?
  end
  
  def test_access_to_the_pool_and_cloud
    pool :box do
      cloud :semaphore do
        has_file :name => "pool_name", :content => pool.name
        has_variable "pool", pool.name
      end
    end
    
    # Purely for testing purposes
    assert_equal "box", clouds["semaphore"].files.first.content
    assert_equal "box", clouds["semaphore"].variables.first.value
    
    assert_equal "box", clouds["semaphore"].variables.first.cloud.files.first.content
    assert_equal "semaphore", clouds["semaphore"].variables.first.cloud.name
  end
  
  def test_get_resource_info
    pool :box do
      cloud :semaphore do
        has_file "pool_name", :content => pool.name
        has_variable "pool", pool.name
      end
    end
    
    assert_equal [  clouds["semaphore"].files.first,
                    clouds["semaphore"].ordered_resources, 
                    0 
                 ], clouds["semaphore"].get_file_info("pool_name")
    assert_equal  clouds["semaphore"].files.first, clouds["semaphore"].get_file("pool_name")
  end

  
end