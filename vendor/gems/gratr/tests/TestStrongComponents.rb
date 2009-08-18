#--
# Copyright (c) 2006 Shawn Patrick Garbett
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

class TestStrongComponents < Test::Unit::TestCase # :nodoc:
  # Test from boost strong_components.cpp
  # Original Copyright 1997-2001, University of Notre Dame.
  # Original Authors: Andrew Lumsdaine, Lie-Quan Lee, Jermey G. Siek
  def test_boost
    g = Digraph[ 'a', 'b',  'a', 'f',  'a', 'h',
                 'b', 'c',  'b', 'a',
                 'c', 'd',  'c', 'b',
                 'd', 'e',
                 'e', 'd',
                 'f', 'g',
                 'g', 'f',  'g', 'd',
                 'h', 'i',
                 'i', 'h',  'i', 'j',  'i', 'e',  'i', 'c']

    c = g.strong_components.map {|x| x.sort}
    assert_equal 10, g.vertices.size
    assert_equal 4, c.size
    assert c.include?(['d','e'])
    assert c.include?(['f','g'])
    assert c.include?(['j'])
    assert c.include?(['a','b','c','h','i'])
  
    cg = g.condensation
    cg_vertices = cg.map {|v| v.sort}
    assert_equal 4, cg_vertices.size
    assert cg_vertices.include?(['j'])
    assert cg_vertices.include?(['d','e'])
    assert cg_vertices.include?(['f', 'g'])
    assert cg_vertices.include?(['a', 'b', 'c', 'h', 'i'])
    assert cg.edges.map {|e| [e.source.sort.join, e.target.sort.join] }.sort ==
           [['abchi','de'], ['abchi', 'fg'], ['abchi', 'j'], ['fg', 'de']]
  end
    

  # Figure #3, from 'Depth-First Search and Linear Graph Algorithms'
  # by Robert Tarjan, SIAM J. Comput. Vol 1, No.2, June 1972
  def test_tarjan_fig_3
    g = Digraph[ 1,2,
                 2,3, 2,8,
                 3,4, 3,7,
                 4,5,
                 5,3, 5,6,
                 7,4, 7,6,
                 8,1, 8,7 ]
                 
    c = g.strong_components.map {|x| x.sort}
    assert_equal 8, g.vertices.size
    assert_equal 3, c.size
    assert c.include?([6])
    assert c.include?([1,2,8]) 
    assert c.include?([3,4,5,7]) 
  end
end
