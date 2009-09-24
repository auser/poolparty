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
    assert_equal({:tester => "hi"}, @inst.get_tester("hi"))
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
  end
  
  def test_does_not_have_a_resource_call
    o = @inst.does_not_have_tester "bob"
    assert !@inst.testers.last.exists?
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
  
  def test_notifies
    pool :testing do
      cloud :in_a_box do
        has_file :name => "pool_name", :content => pool.name
        has_exec "echo 'hello'", :notifies => get_file("pool_name")
      end
    end
    
    assert_equal({:file => [["pool_name", :reload, :delayed]]}, clouds["in_a_box"].execs.first.meta_notifies)
  end
  
  def test_subscribes_hash
    clear!
    pool :testing do
      cloud :in_a_box_for_subscribes do
        has_file :name => "pool_name", :content => pool.name
        has_exec "echo 'hello'" do
          subscribes get_file("pool_name"), :reload, :immediately
        end
      end
    end
    
    assert_equal({:file=>[["pool_name", :reload, :immediately]]}, clouds["in_a_box_for_subscribes"].execs.first.meta_subscribes)
    
  end
    
  def test_required_resourcing
    pool :square do
      cloud :fighting do
        has_user "ari"
        has_directory "/etc/dir", :requires => {:user => "ari", :file => "/etc/my_configs"}
        has_file "/etc/my_configs"
      end
    end
    
    # p clouds["semaphore"].files
    assert_equal clouds["fighting"].get_resource(:file, "/etc/my_configs"), clouds["fighting"].files.first
    
    if verbose?
      clouds["fighting"].output_resources_graph('png', "graph", {"fontsize" => 30})
      assert File.file?("#{File.dirname(__FILE__)}/graph.dot")
      File.unlink("#{File.dirname(__FILE__)}/graph.png")
      File.unlink("#{File.dirname(__FILE__)}/graph.dot")
    end
  end
  
  def test_a_subclassed_resource_has_the_method_of_the_subclassed_resource
    pool "oblong" do
      cloud "piece" do
        fake_plugin do
          has_subclassed "box"
        end
      end
    end
    
    assert_equal PoolParty::Resources::FakePlugin, clouds["piece"].resources.first.class
    assert_equal PoolParty::Resources::FakeSubclassedPlugin, clouds["piece"].resources.first.resources[0].class
    assert_equal 1, clouds["piece"].resources.first.resources[0].resources.size
  end
  
end