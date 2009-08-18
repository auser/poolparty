#!/usr/bin/ruby

# Priority Queue tests

$:.unshift '../ext/priority_queue/CPriorityQueue'
$:.unshift '../lib/'

require 'test/unit'

require 'priority_queue/ruby_priority_queue'
require 'priority_queue/poor_priority_queue'
begin
  require 'priority_queue/CPriorityQueue' 
rescue LoadError
  require 'CPriorityQueue'
end


module PriorityQueueTest    
  # Check that the order is maintained
  def teardown
    last = @q.min_priority
    while priority = @q.delete_min_return_priority
      assert_operator(last, :<=, priority)
      last = priority
    end
  end

  def test_length
    20.times do | i |
      assert_equal(i, @q.length)
      @q[i] = i
    end
    10.times do | i |
      assert_equal(20-i, @q.length)
      @q.delete_min
    end
    10.times do | i |
      assert_equal(10+i, @q.length)
      @q[i] = i
    end
    @q.delete(5)
    assert_equal(19, @q.length)
  end

  def test_merge
    @q1 = @q.class.new
    @q2 = @q.class.new

    20.times do | i |
      @q1[i] = i
      @q2[i+20] = i+20
    end
  end

  # Assure that delete min works
  def test_delete_min
    assert_equal(nil, @q.delete_min, "Empty queue should pop nil")
    @q["n1"] = 0
    assert_equal(["n1", 0], @q.delete_min)
    @q["n1"] = 0
    @q["n2"] = -1
    assert_equal(["n2", -1], @q.delete_min)
  end

  def test_delete_min_return_key
    assert_equal(nil, @q.delete_min_return_key, "Empty queue should pop nil")
    @q["n1"] = 0
    assert_equal("n1", @q.delete_min_return_key)
    @q["n1"] = 0
    @q["n2"] = -1
    assert_equal("n2", @q.delete_min_return_key)
  end

  def test_delete_min_return_priority
    assert_equal(nil, @q.delete_min_return_priority, "Empty queue should pop nil")
    @q["n1"] = 0
    assert_equal(0, @q.delete_min_return_priority)
    @q["n1"] = 0
    @q["n2"] = -1
    assert_equal(-1, @q.delete_min_return_priority)
  end

  def test_has_key?
    assert(!@q.has_key?(1))
    @q[1] = 1
    assert(@q.has_key?(1))
  end

  def test_empty?
    assert_equal(true, @q.empty?, "Empty queue should return true on empty?")
    @q["node1"] = 10
    assert_equal(false, @q.empty?, "Filled queue should return false on empty?")
  end

  def test_push
    20.times do | i |
      @q.push i, i+10
    end

    20.times do | i |
      @q.push i, i
    end
    
    20.times do | i |
      assert_equal([i, i], @q.delete_min)
    end

    assert_equal(nil, @q.delete_min)
  end

  def test_push_pop
    20.times do | i |
      @q.push i, i
    end

    20.times do | i |
      assert_equal([i, i], @q.delete_min)
    end

    assert_equal(nil, @q.delete_min)
  end

  def test_push_decrease_pop
    50.times do | i |
      @q.push i, i
    end

    10.times do | i |
      assert_equal([i, i], @q.delete_min)
    end

    10.times do | i |
      @q[i+10] = i
    end

    10.times do | i |
      assert_equal([i+10, i], @q.delete_min)
    end

    30.times do | i |
      assert_equal([i+20, i+20], @q.delete_min)
    end

    assert_equal(nil, @q.delete_min)
  end

  def test_min_key
    assert_equal(nil, @q.min_key)
    @q["node1"] = 0
    assert_equal("node1", @q.min_key)
    @q["node2"] = 1
    assert_equal("node1", @q.min_key)
    @q["node3"] = -1
    assert_equal("node3", @q.min_key)
  end

  def test_min_priority
    assert_equal(nil, @q.min_priority)
    @q["node1"] = 0
    assert_equal(0, @q.min_priority)
    @q["node2"] = 1
    assert_equal(0, @q.min_priority)
    @q["node3"] = -1
    assert_equal(-1, @q.min_priority)
  end

  def test_access
    assert_equal(0, @q["node1"] = 0)
    assert_equal(["node1", 0], @q.min)
    assert_equal(1, @q["node2"] = 1)
    assert_equal(1, @q["node2"])
    assert_equal("node1", @q.min_key)
    assert_equal(2, @q["node3"] = 2)
    assert_equal(2, @q["node3"])
    assert_equal("node1", @q.min_key)
    assert_equal(-1, @q["node3"] = -1)
    assert_equal(-1, @q["node3"])
    assert_equal("node3", @q.min_key)
  end

  def test_min
    assert_equal(nil, @q.min)
    @q["node1"] = 10
    assert_equal(["node1", 10], @q.min)
    @q["node2"] = 5
    assert_equal(["node2", 5], @q.min)
  end

  def test_decrease_priority

    20.times do | i |
      @q.push i, i / 20.0
    end

    assert_equal([0, 0], @q.delete_min)

    @q[10] = -1
    @q[11] = -0.5

    [10, 11, (1..9).to_a, (12..19).to_a, nil].flatten.each do | shall |
      key, priority = *@q.delete_min
      assert_equal(shall, key)
    end
  end

  def test_increase_priority
    20.times do | i |
      @q[i] = i
    end
    @q[10] = 5
    assert_equal([0,0], @q.delete_min)
    assert_equal(10, @q[10] = 10)
    assert_equal(20, @q[11] = 20)
    assert_equal([1,1], @q.delete_min)
  end

  def test_delete
    @q[1] = 1
    @q[2] = 2
    @q[3] = 3
    assert_equal(1, @q[1])
    assert_equal([1,1], @q.min)
    assert_equal([1,1], @q.delete(1))
    assert_equal(nil, @q[1])
    assert_equal([2,2], @q.min)
    assert_equal(nil, @q.delete(1))
  end

  def test_example_1
    assert_equal(0, @q["node1"] = 0)
    assert_equal(1, @q["node2"] = 1)
    assert_equal("node1", @q.min_key)
    assert_equal(0, @q[@q.min_key])
    assert_equal(0, @q.min_priority)

    @q["node2"] = -1
    assert_equal(["node2", -1], @q.delete_min)
    assert_equal(nil, @q["node2"])
    @q["node3"] = 1

    assert_equal(["node3", 1], @q.delete("node3"))
    assert_equal(nil, @q.delete("node2"))
  end

  def test_dup
    ('a'..'z').each do | n |
      @q[n] = n[0]
    end
    qq = @q.dup
    until @q.empty?
      assert_equal(@q.delete_min, qq.delete_min)
    end
  end

  def test_each
    ('a'..'z').each do | n |
      @q[n] = n[0]
    end
    queue = ('a'..'z').inject([]) { | r, n | r << [n, n[0]] }
    assert_equal(queue.sort, @q.to_a.sort)
  end

  extend self
end

class CPriorityQueueTest < Test::Unit::TestCase
  include PriorityQueueTest

  def setup
    @q = CPriorityQueue.new
  end

  def test_to_dot    
    5.times do | i |
      @q.push "N#{i}", i
    end
    @q.delete_min
    assert_equal(
    ['digraph fibonacci_heap {',
    '  NODE [label="N1 (1)",shape=box];',
    '    NODE [label="N3 (3)",shape=box];',
    '      NODE [label="N4 (4)",shape=box];',
    '    NODE -> NODE;',
    '  NODE -> NODE;',
    '    NODE [label="N2 (2)",shape=box];',
    '  NODE -> NODE;',
    '}',''].join("\n"),  @q.to_dot.gsub(/NODE[0-9]*/, 'NODE'))
  end

end

class PoorPriorityQueueTest < Test::Unit::TestCase
  include PriorityQueueTest

  def setup
    @q = PoorPriorityQueue.new
  end

end

class RubyPriorityQueueTest < Test::Unit::TestCase
  include PriorityQueueTest

  def setup
    @q = RubyPriorityQueue.new
  end

  def test_private_link_nodes
    q = RubyPriorityQueue.new
    q[0] = 0
    q[1] = 1
    tc = self
    q.instance_eval do
      n0 = @nodes[0]
      n1 = @nodes[1]
      n0.right = n0.left = n0
      n1.right = n1.left = n1
      tc.assert_equal(n0, link_nodes(n0, n1))
      tc.assert_equal(n0.child, n1)
      tc.assert_equal(n1.child, nil)
      tc.assert_equal(n0.left, n0)
      tc.assert_equal(n1.left, n1)
      tc.assert_equal(n0.right, n0)
      tc.assert_equal(n1.right, n1)
    end
    q = RubyPriorityQueue.new
    q[0] = 0
    q[1] = 1
    q.instance_eval do
      n0 = @nodes[0]
      n1 = @nodes[1]
      n0.right = n0.left = n0
      n1.right = n1.left = n1
      tc.assert_equal(n0, link_nodes(n1, n0))
      tc.assert_equal(n0.child, n1)
      tc.assert_equal(n1.child, nil)
      tc.assert_equal(n0.left, n0)
      tc.assert_equal(n1.left, n1)
      tc.assert_equal(n0.right, n0)
      tc.assert_equal(n1.right, n1)
    end
  end


  def test_private_delete_first
    q = RubyPriorityQueue.new
    q[0] = 0
    q[1] = 1
    q[2] = 2
    tc = self
    q.instance_eval do
      2.times do
	r = @rootlist
	tc.assert_equal(r, delete_first)
	tc.assert_equal(r.right, r)
	tc.assert_equal(r.left, r)
	tc.assert_not_equal(r, @rootlist.left)
	tc.assert_not_equal(r, @rootlist.right)
      end
      r = @rootlist
      tc.assert_equal(r, delete_first)
      tc.assert_equal(r.right, r)
      tc.assert_equal(r.left, r)
      tc.assert_equal(nil, @rootlist)

      tc.assert_equal(nil, delete_first)
    end
  end
end

