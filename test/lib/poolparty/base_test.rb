require "#{File.dirname(__FILE__)}/../../test_helper"

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
  
  def test_resource_graph
    assert_equal RGL::DirectedAdjacencyGraph, inst.resources_graph.class
    assert_equal %w(c b a), inst.ordered_resources.map {|a| a.name }
  end
  
end