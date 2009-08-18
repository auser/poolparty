#--
# Copyright (c) 2006 Shawn Patrick Garbett
# Copyright (c) 2002,2004,2005 by Horst Duchene
# 
# Redistribution and use in source and binary forms, with or without modification,
# are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright notice(s),
#       this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright notice,
#       this list of conditions and the following disclaimer in the documentation
#       and/or other materials provided with the distribution.
#     * Neither the name of the Shawn Garbett nor the names of its contributors
#       may be used to endorse or promote products derived from this software
#       without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#++


require 'test/unit'
require 'gratr/import'

class TestUndirectedGraph < Test::Unit::TestCase # :nodoc:

  def setup
    @single = UndirectedGraph[1,2, 2,3, 3,4, 4,4, 1,2, 2,3]
    @dups   = UndirectedPseudoGraph[1,2, 2,3, 3,4, 4,4, 1,2, 2,3]
    @loops  = UndirectedMultiGraph[1,2, 2,3, 3,4, 4,4, 1,2, 2,3]
  end

  def test_new
    assert_equal UndirectedGraph[1,2, 2,3, 3,4, 4,4], @single
    assert_equal UndirectedPseudoGraph[1,2, 2,3, 3,4, 4,4, 1,2, 2,3], @dups
    assert_equal UndirectedMultiGraph[1,2, 2,3, 3,4, 4,4, 1,2, 2,3], @loops
    assert_raise(ArgumentError) {UndirectedGraph.new(:bomb)}
    assert_raise(ArgumentError) {UndirectedGraph.new(1)}
    assert_equal @single, UndirectedGraph.new(@single)
  end

  def test_edges
    assert @single.edges.include?(Edge[1,2])
    assert @single.edges.include?(Edge[2,3])
    assert @single.edges.include?(Edge[3,4])
    assert !@single.edges.include?(Edge[4,4])
    assert @loops.edges.include?(MultiEdge[4,4])
    assert @single.edges.include?(Edge[1,2])
    assert @single.edges.include?(Edge[2,3])
    assert !@single.edges.include?(Edge[1,3])
    assert @single.edge?(2,3)
    assert !@single.edge?(1,4)
    assert @single.edge?(Edge[1,2])
    assert !@single.add_edge!(5,5).edge?(5,5)
    assert !@dups.add_edge!(5,5).edge?(5,5)
    assert @loops.add_edge!(5,5).edge?(5,5)
    assert !@single.remove_edge!(5,5).edge?(5,5)
  end

  def test_vertices
    assert_equal [1,2,3,4],   @single.vertices.sort
    assert_equal [1,2,3,4,5], @single.add_vertex!(5).sort
    assert_equal [1,2,4,5],   @single.remove_vertex!(3).sort
    assert !@single.vertex?(3)
    assert !@single.edge?(2,3)
    assert !@single.edge?(3,4)
  end

  def test_properties
    assert !@single.directed?
    assert @single.empty? == false
    assert UndirectedGraph.new.empty? == true
    assert_equal 4, @single.size 
    assert_equal 4, @dups.size 
    assert_equal 4, @single.num_vertices
    assert_equal 4, @dups.num_vertices
    assert_equal 3, @single.num_edges
    assert_equal 6, @loops.num_edges
    assert_equal 5, @dups.num_edges
  end

  def test_merge
    @dups.merge(@single)
    assert_equal 8, @dups.num_edges
    assert_equal [1,2,3,4],   @dups.vertices.sort
  end

  def test_operators
    result = @single + Edge[3,2] 
    assert_equal 4, @single.size 
    assert_equal 3, @single.num_edges
    assert_equal 4, result.size 
    assert_equal 3, result.num_edges

    result = @single + 5
    assert_equal 4, @single.size 
    assert_equal 3, @single.num_edges
    assert_equal 5, result.size 
    assert_equal 3, result.num_edges

    result = @single - Edge[4,4]
    assert_equal 4, @single.size 
    assert_equal 3, @single.num_edges
    assert_equal 4, result.size 
    assert_equal 3, result.num_edges

    result = @single - 4
    assert_equal 4, @single.size
    assert_equal 3, @single.num_edges
    assert_equal 3, result.size 
    assert_equal 2, result.num_edges
    
    @single << Edge[6,1]
    assert_equal 5, @single.size
    assert_equal 4, @single.num_edges
    assert @single.edge?(6,1)
  end

  def test_complement
    complement = @single.complement 
    assert [1,2,3,4], complement.vertices.sort
    assert !complement.edge?(1,1)
    assert complement.edge?(1,3)
    assert complement.edge?(1,4)
    assert !complement.edge?(2,2)
    assert complement.edge?(2,4)
    assert complement.edge?(3,1)
    assert !complement.edge?(3,3)
    assert complement.edge?(4,1)
    assert complement.edge?(4,2)
    assert 7, complement.num_edges
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
    assert @single.include?(Edge[1,2])
    assert @dups.include?(Edge[1,2])
  end

  def test_adjacent

    assert @single.adjacent?(2, Edge[1,2])
    assert_equal [2], @single.adjacent(1)

    assert_equal [Edge[1,2]], @single.adjacent(1, :type=>:edges)
    assert_equal [Edge[1,2]], @single.adjacent(1, :type=>:edges, :direction=> :out)
    assert_equal [Edge[1,2],Edge[2,3]], @single.adjacent(2, :type=>:edges, :direction=> :in).sort
    assert_equal [Edge[1,2],Edge[2,3]], @single.adjacent(2, :type=>:edges, :direction=> :all).sort

    assert_equal [MultiEdge[1,2]]*2, @dups.adjacent(1, :type=>:edges)
    assert_equal [MultiEdge[1,2]]*2, @dups.adjacent(1, :type=>:edges, :direction=> :out)
    assert_equal ([MultiEdge[1,2]]*2 + [MultiEdge[2,3]]*2), @dups.adjacent(2, :type=>:edges, :direction=> :in).sort
    assert_equal ([MultiEdge[1,2]]*2 + [MultiEdge[2,3]]*2), @dups.adjacent(2, :type=>:edges, :direction=> :all).sort

    assert_equal [2], @single.adjacent(1, :type=>:vertices)
    assert_equal [2], @single.adjacent(1, :type=>:vertices, :direction=> :out)
    assert_equal [1,3], @single.adjacent(2, :type=>:vertices, :direction=> :in)
    assert_equal [1,3], @single.adjacent(2, :type=>:vertices, :direction=> :all)

    assert_equal [2,3], @single.adjacent(Edge[2,3], :type=>:vertices)
    assert_equal [2,3], @single.adjacent(Edge[2,3], :type=>:vertices, :direction=> :out)
    assert_equal [2,3], @single.adjacent(Edge[2,3], :type=>:vertices, :direction=> :in)
    assert_equal [2,3], @single.adjacent(Edge[2,3], :type=>:vertices, :direction=> :all)

    assert_equal [Edge[1,2],Edge[3,4]], @single.adjacent(Edge[2,3], :type=>:edges).sort
    assert_equal [Edge[1,2],Edge[3,4]], @single.adjacent(Edge[2,3], :type=>:edges, :direction=> :out).sort
    assert_equal [Edge[1,2],Edge[3,4]], @single.adjacent(Edge[2,3], :type=>:edges, :direction=> :in).sort
    assert_equal [Edge[1,2],Edge[3,4]], @single.adjacent(Edge[2,3], :type=>:edges, :direction=> :all).sort
    assert_equal ([MultiEdge[1,2]]*2 + [MultiEdge[3,4]]), @dups.adjacent(MultiEdge[2,3], :type=>:edges).sort
    assert_equal ([MultiEdge[1,2]]*2 + [MultiEdge[3,4]]), @dups.adjacent(MultiEdge[2,3], :type=>:edges, :direction=>:out).sort
    assert_equal ([MultiEdge[1,2]]*2 + [MultiEdge[3,4]]), @dups.adjacent(MultiEdge[2,3], :type=>:edges, :direction=>:in).sort
    assert_equal ([MultiEdge[1,2]]*2+[MultiEdge[3,4]]), @dups.adjacent(MultiEdge[2,3], :type=>:edges, :direction=> :all).sort
  end

  def test_neighborhood
    assert_equal [2],    @single.neighborhood(1).sort
    assert_equal [1,3],  @single.neighborhood(2).sort
    assert_equal [Edge[1,2], Edge[3,4]], @single.neighborhood(Edge[2,3]).sort
  end

  def test_degree
    assert_equal 1, @single.in_degree(1)
    assert_equal 2, @single.in_degree(2)
    assert_equal 1, @single.in_degree(4)
    assert_equal 1, @single.out_degree(1)
    assert_equal 2, @single.out_degree(2)
    assert_equal 1, @single.out_degree(4)
    assert_equal 0, @single.add_vertex!(6).out_degree(6)
    assert_equal 0, @single.add_vertex!(7).in_degree(7)
    assert_equal 2, @single.add_edge!(4,2).out_degree(4)
    assert_equal 3, @single.in_degree(2)
  end

  def test_include
    assert @single.include?(2)
    assert !@single.include?(23)
    assert @single.include?(Edge[1,2])
    assert !@single.include?(Edge[1,4])
  end

end
