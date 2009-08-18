#--
# Copyright (c) 2006 Shawn Patrick Garbett
# Copyright (c) 2002,2004,2005 by Horst Duchene
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#++

require 'test/unit'
require 'gratr/import'

class TestDigraph < Test::Unit::TestCase # :nodoc:

  def setup
    @single = Digraph[1,2, 2,3, 3,4, 1,2, 2,3, 4,4]
    @dups   = DirectedPseudoGraph[1,2, 2,3, 3,4, 1,2, 2,3, 4,4]
    @loops  = DirectedMultiGraph[1,2, 2,3, 3,4, 4,4, 1,2, 2,3]
  end

  def test_new
    assert_equal Digraph[1,2, 2,3, 3,4], @single
    assert_equal DirectedPseudoGraph.new([1,2, 2,3, 3,4, 1,2, 2,3]), @dups
    assert_equal DirectedMultiGraph.new([1,2, 2,3, 3,4, 4,4, 1,2, 2,3]), @loops
    assert_raise(ArgumentError) {Digraph.new(:loops)}
    assert_raise(ArgumentError) {Digraph.new(:parallel_edges)}
    assert_raise(ArgumentError) {DirectedMultiGraph.new(:loops)}
    assert_raise(ArgumentError) {DirectedMultiGraph.new(:parallel_edges)}
    assert_raise(ArgumentError) {DirectedPseudoGraph.new(:loops)}
    assert_raise(ArgumentError) {DirectedPseudoGraph.new(:parallel_edges)}
    assert_raise(ArgumentError) {Digraph.new(1)}
    assert_equal @single, Digraph.new(@single)
    assert_equal @dups, DirectedPseudoGraph.new(@dups)
    assert_equal @loops, DirectedMultiGraph.new(@loops)
    assert_equal Digraph[1,2, 2,3, 3,4], Digraph.new(@loops)
  end

  def test_edges
    assert_equal 3,@single.edges.size 
    assert @single.edges.include?(Arc[1,2])
    assert @single.edges.include?(Arc[2,3])
    assert @single.edges.include?(Arc[3,4])
    assert !@single.edges.include?(Arc[4,4])
    assert @single.edges.include?(Arc[1,2])
    assert @single.edges.include?(Arc[2,3])
    assert !@single.edges.include?(Arc[1,3])
    assert @single.edge?(2,3)
    assert !@single.edge?(1,4)
    assert @single.edge?(Arc[1,2])
    assert !@single.add_edge!(5,5).edge?(5,5)
    assert !@single.remove_edge!(5,5).edge?(5,5)

    assert_equal 5,@dups.edges.size 
    assert @dups.edges.include?(MultiArc[1,2])
    assert @dups.edges.include?(MultiArc[2,3])
    assert @dups.edges.include?(MultiArc[3,4])
    assert !@dups.edges.include?(MultiArc[4,4])
    assert @dups.edges.include?(MultiArc[1,2])
    assert @dups.edges.include?(MultiArc[2,3])
    assert !@dups.edges.include?(MultiArc[1,3])
    assert @dups.edge?(2,3)
    assert !@dups.edge?(1,4)
    assert @dups.edge?(MultiArc[1,2])
    assert !@dups.add_edge!(5,5).edge?(5,5)
    assert_raise(ArgumentError) { @dups.remove_edge!(5,5) }

    assert_equal 5,@dups.edges.size 
    assert @loops.edges.include?(MultiArc[1,2])
    assert @loops.edges.include?(MultiArc[2,3])
    assert @loops.edges.include?(MultiArc[3,4])
    assert @loops.edges.include?(MultiArc[4,4])
    assert @loops.edges.include?(MultiArc[1,2])
    assert @loops.edges.include?(MultiArc[2,3])
    assert !@loops.edges.include?(MultiArc[1,3])
    assert @loops.edge?(2,3)
    assert !@loops.edge?(1,4)
    assert @loops.edge?(MultiArc[1,2])
    assert @loops.add_edge!(5,5).edge?(5,5)
    assert_raise(ArgumentError) { @loops.remove_edge!(5,5) }

  end

  def test_vertices
    assert_equal [1,2,3,4],   @single.vertices.sort
    assert_equal [1,2,3,4,5], @single.add_vertex!(5).sort
    assert_equal [1,2,4,5],   @single.remove_vertex!(3).sort
    assert !@single.vertex?(3)
    assert !@single.edge?(2,3)
    assert !@single.edge?(3,4)
    assert @single.add_vertex(:bogus).vertex?(:bogus)
    assert !@single.add_vertex(:bogus).vertex?(nil)
    assert !@single.vertex?(:bogus)
    @single.add_vertex!(:real)
    assert @single.vertex?(:real)
    assert @single.add_edge(:here, :there).edge?(Arc[:here, :there])
    assert !@single.edge?(Arc[:here, :there])
    assert !@single.vertex?(:here)
    assert !@single.vertex?(:there)
    @single.add_edge!(:here, :there)
    assert @single.edge?(Arc[:here, :there])
    assert @single.vertex?(:here)
    assert @single.vertex?(:there)
  end

  def test_properties
    assert @single.directed?
    assert @single.empty? == false
    assert Digraph.new.empty? == true
    assert_equal 4, @single.size 
    assert_equal 4, @dups.size 
    assert_equal 4, @loops.size 
    assert_equal 4, @single.num_vertices
    assert_equal 4, @dups.num_vertices
    assert_equal 3, @single.num_edges
    assert_equal 5, @dups.num_edges
    assert_equal 6, @loops.num_edges
    assert @single.oriented?
    @single.remove_vertex!(4)
    assert @single.oriented?
    assert !@loops.oriented?
    @loops.remove_vertex!(4)
    assert @loops.oriented?
  end

  def test_merge
    @dups.merge(@single)
    assert_equal 8, @dups.num_edges
    assert_equal [1,2,3,4],   @dups.vertices.sort
  end

  def test_operators
    result = @single + Arc[3,2] 
    assert_equal 4, @single.size 
    assert_equal 3, @single.num_edges
    assert_equal 4, result.size 
    assert_equal 4, result.num_edges

    result = @single + 5
    assert_equal 4, @single.size 
    assert_equal 3, @single.num_edges
    assert_equal 5, result.size 
    assert_equal 3, result.num_edges

    result = @single - Arc[4,4]
    assert_equal 4, @single.size 
    assert_equal 3, @single.num_edges
    assert_equal 4, result.size 
    assert_equal 3, result.num_edges

    e = @loops.edges.detect{|e| e.source == 4 and e.target == 4}
    result = @loops - e
    assert_equal 4, @single.size 
    assert_equal 3, @single.num_edges
    assert_equal 4, result.size 
    assert_equal 5, result.num_edges

    result = @single - 4
    assert_equal 4, @single.size
    assert_equal 3, @single.num_edges
    assert_equal 3, result.size 
    assert_equal 2, result.num_edges
    
    @single << Arc[6,1]
    assert_equal 5, @single.size
    assert_equal 4, @single.num_edges
    assert @single.edge?(6,1)
  end

  def test_reversal
    reverse = @single.add_vertex!(42).reversal
    assert_equal [1,2,3,4,42], reverse.vertices.sort
    assert reverse.edge?(2,1)
    assert reverse.edge?(3,2)
    assert reverse.edge?(4,3)
    assert !reverse.edge?(4,4)
    assert_equal 3, reverse.num_edges
    reverse = @loops.reversal
    assert reverse.edge?(4,4)
  end

  def test_complement
    complement = @single.complement 
    assert_equal [1,2,3,4], complement.vertices.sort
    assert !complement.edge?(1,1)
    assert complement.edge?(1,3)
    assert complement.edge?(1,4)
    assert complement.edge?(2,1)
    assert complement.edge?(2,4)
    assert complement.edge?(3,1)
    assert complement.edge?(3,2)
    assert complement.edge?(4,1)
    assert complement.edge?(4,2)
    assert complement.edge?(4,3)
    assert 9, complement.num_edges

    complement = @loops.complement 
    assert_equal [1,2,3,4], complement.vertices.sort
    assert complement.edge?(1,1)
    assert complement.edge?(1,3)
    assert complement.edge?(1,4)
    assert complement.edge?(2,1)
    assert complement.edge?(2,2)
    assert complement.edge?(2,4)
    assert complement.edge?(3,1)
    assert complement.edge?(3,2)
    assert complement.edge?(3,3)
    assert complement.edge?(4,1)
    assert complement.edge?(4,2)
    assert complement.edge?(4,3)
    assert 12, complement.num_edges
  end

  def test_induced_subgraph
    induced = @single.induced_subgraph([1,2])
    assert [1,2], induced.vertices.sort
    assert induced.edge?(1,2)
    assert 1, induced.num_edges
  end

  def test_include
    assert @single.include?(4)
    assert @dups.include?(4)
    assert !@dups.include?(5)
    assert !@single.include?(5)
    assert @single.include?(Arc[1,2])
    assert @dups.include?(Arc[1,2])
  end

  def test_adjacent

    assert @single.adjacent?(2, Arc[1,2])
    assert_equal [2], @single.adjacent(1)

    assert_equal [Arc[1,2]], @single.adjacent(1, :type=>:edges)
    assert_equal [Arc[1,2]], @single.adjacent(1, :type=>:edges, :direction=> :out)
    assert_equal [Arc[1,2]], @single.adjacent(2, :type=>:edges, :direction=> :in)
    assert_equal [Arc[1,2],Arc[2,3]], @single.adjacent(2, :type=>:edges, :direction=> :all).sort

    [[{},1], [{:direction => :out},1], [{:direction => :in},2]].each do |h,v|
      adj = @dups.adjacent(v, h.merge(:type=>:edges))
      assert_equal 2, adj.size
      adj.each {|e| assert e.source == 1; assert e.target == 2}
    end 
    
    adj = @dups.adjacent(2, {:type=>:edges,:direction=>:all})
    assert_equal 4, adj.size
    adj.each do |e| 
      assert((e.source==1 and e.target==2) ||
             (e.source==2 and e.target==3)) 
    end
    
    assert_equal [2],   @single.adjacent(1, :type=>:vertices)
    assert_equal [2],   @single.adjacent(1, :type=>:vertices, :direction=> :out)
    assert_equal [1],   @single.adjacent(2, :type=>:vertices, :direction=> :in)
    assert_equal [1,3], @single.adjacent(2, :type=>:vertices, :direction=> :all)

    assert_equal [3], @single.adjacent(Arc[2,3], :type=>:vertices)
    assert_equal [3], @single.adjacent(Arc[2,3], :type=>:vertices, :direction=> :out)
    assert_equal [2], @single.adjacent(Arc[2,3], :type=>:vertices, :direction=> :in)
    assert_equal [2,3], @single.adjacent(Arc[2,3], :type=>:vertices, :direction=> :all)

    assert_equal [Arc[3,4]], @single.adjacent(Arc[2,3], :type=>:edges)
    assert_equal [Arc[3,4]], @single.adjacent(Arc[2,3], :type=>:edges, :direction=> :out)
    assert_equal [Arc[1,2]], @single.adjacent(Arc[2,3], :type=>:edges, :direction=> :in)
    assert_equal [Arc[1,2],Arc[3,4]], @single.adjacent(Arc[2,3], :type=>:edges, :direction=> :all).sort
    
    assert_equal [MultiArc[3,4]], @dups.adjacent(MultiArc[2,3], :type=>:edges)
    assert_equal [MultiArc[3,4]], @dups.adjacent(MultiArc[2,3], :type=>:edges, :direction=> :out)
    assert_equal [MultiArc[1,2]]*2, @dups.adjacent(MultiArc[2,3], :type=>:edges, :direction=> :in)
    assert_equal ([MultiArc[1,2]]*2+[MultiArc[3,4]]), @dups.adjacent(MultiArc[2,3], :type=>:edges, :direction=> :all).sort
  end

  def test_neighborhood
    assert_equal [2],    @single.neighborhood(1).sort
    assert_equal [1,3],  @single.neighborhood(2).sort
    assert_equal [Arc[1,2], Arc[3,4]], @single.neighborhood(Arc[2,3]).sort
  end

  def test_degree
    assert_equal 0, @single.in_degree(1)
    assert_equal 1, @single.in_degree(2)
    assert_equal 1, @single.in_degree(4)
    assert_equal 3, @loops.degree(4)
    assert_equal 2, @loops.in_degree(4)
    assert_equal 1, @single.out_degree(1)
    assert_equal 1, @single.out_degree(2)
    assert_equal 0, @single.out_degree(4)
    assert_equal 1, @loops.out_degree(4)
    assert_equal 0, @single.add_vertex!(6).out_degree(6)
    assert_equal 0, @single.add_vertex!(7).in_degree(7)
    assert_equal 1, @single.add_edge!(4,2).out_degree(4)
    assert_equal 2, @loops.add_edge!(4,2).out_degree(4)
    assert_equal 2, @single.in_degree(2)

    assert_equal 0, @single.min_in_degree
    assert_equal 2, @single.max_in_degree
    assert_equal 0, @single.min_out_degree
    assert_equal 1, @single.max_out_degree

    assert_equal 0, @loops.min_in_degree
    assert_equal 2, @loops.max_in_degree
    assert_equal 1, @loops.min_out_degree
    assert_equal 2, @loops.max_out_degree
    assert_equal 4, @loops.degree(2)
    assert_equal 1, @single.degree(1)
    assert !@loops.regular?
    assert !@single.regular?
    assert !@dups.regular?
  end

  def test_include
    assert @single.include?(2)
    assert !@single.include?(23)
    assert @single.include?(Arc[1,2])
    assert !@single.include?(Arc[1,4])
  end

end
