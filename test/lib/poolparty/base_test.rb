require "#{File.dirname(__FILE__)}/../../test_helper"

include_fixture_resources

class BaseTestClass < PoolParty::Base  
  default_options :a => "a", :d => "dump"
end

class BaseTest < Test::Unit::TestCase
  
  def test_compile_opts
    assert_equal BaseTestClass.new(:bob).init_opts, {:name => "bob"}
    assert_equal BaseTestClass.new(:name => "Santra").init_opts, {:name => "Santra"}
    assert_equal BaseTestClass.new(:apples, :friend => "oranges").init_opts, {:name => "apples", :friend => "oranges"}
  end
  
  def inst
    return @inst if @inst
    inst = BaseTestClass.new
    inst.resources << BaseTestClass.new(:a)
    @b = BaseTestClass.new(:b)
    @b.resources << BaseTestClass.new(:c)
    inst.resources << @b
    @inst = inst
  end
  
  def test_all_resources
    assert_equal %w(a b c), inst.all_resources.map {|r| r.name }
    assert_equal %w(c), @b.all_resources.map {|r| r.name }
  end
  
  def test_resource_graph_generation
    assert_equal GRATR::Digraph, inst.resources_graph.class
  end
  
  def test_instantiation
    PoolParty::Resource.define_resource_methods
    assert_equal "string_name", FakeResource.new.has_tester("string_name").name
    assert_equal "opts_name", FakeResource.new.has_tester(:name => "opts_name").name
    i = FakeResource.new
    i.has_tester do 
      self.name "block_name"
    end
    assert_equal "block_name", i.testers.first.name
  end
  
end