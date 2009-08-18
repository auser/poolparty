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

class TestArc < Test::Unit::TestCase # :nodoc:

  def setup
    @e = Arc.new(1,2,'boo')
    @u = Edge.new(1,2,'hoo')
  end

  def test_edge_new
    assert_raises(ArgumentError) {Arc.new}
    assert_raises(ArgumentError) {Arc.new(1)}
    assert Arc.new(1,2)
    assert Arc.new(1,2,'label')
  end

  def test_edge_getters

    assert_equal(1,     @e.source)
    assert_equal(2,     @e.target)
    assert_equal('boo', @e.label)

    assert_equal(1,     @e[0])
    assert_equal(2,     @e[1])
    assert_equal('boo', @e[2])

    assert_equal(1,     @e[-3])
    assert_equal(2,     @e[-2])
    assert_equal('boo', @e[-1])

    assert_raise(IndexError) {@e[-4]} 
    assert_raise(IndexError) {@e[3]}  

    assert_equal(1,     @e['source'])
    assert_equal(2,     @e['target'])
    assert_equal('boo', @e['label'])

    assert_equal(1,     @e[:source])
    assert_equal(2,     @e[:target])
    assert_equal('boo', @e[:label])
  end

  def test_edge_setters
    @e.source = 23
    @e.target = 42
    @e.label  = 'Yabba'
    assert_equal(23,     @e.source)
    assert_equal(42,     @e.target)
    assert_equal('Yabba',@e.label)

    @e['source'] = 2
    @e['target'] = 1
    @e['label']  = 'Dabba'
    assert_equal(2,      @e.source)
    assert_equal(1,      @e.target)
    assert_equal('Dabba',@e.label)

    @e[:source] = 9
    @e[:target] = 8
    @e['label'] = 'Doooo!'
    assert_equal(9,       @e.source)
    assert_equal(8,       @e.target)
    assert_equal('Doooo!',@e.label)

    @e[0] = 'Fred'
    @e[1] = 'Flintstone'
    @e[2] = 'and'
    assert_equal('Fred',      @e.source)
    assert_equal('Flintstone',@e.target)
    assert_equal('and',       @e.label)

    @e[-3] = 'Barney'
    @e[-2] = 'Rubble'
    @e[-1] = nil
    assert_equal('Barney',    @e.source)
    assert_equal('Rubble',    @e.target)
    assert_equal(nil,         @e.label)
  end

  def test_edge_simple_methods
    assert_equal([1,2,'boo'], @e.to_a)
    assert_equal("(1-2 'boo')", @e.to_s)
    @e.label = nil
    assert_equal("(1-2)", @e.to_s)
    assert(@e.eql?(Arc.new(1,2)))
    assert(!@e.eql?(Arc.new(1,3)))
    assert(!Arc.new(2,1).eql?(@e))

    assert(@e             == (Arc.new(1,2)))
    assert(@e.reverse     == (Arc.new(2,1)))
    assert(Arc.new(1,2)  != (Arc.new(1,3)))
    assert(Arc.new(2,1)  != @e)
  end

  def test_edge_sort
    x = [ Arc.new(2,3), Arc.new(1,3), Arc.new(1,2), Arc.new(2,1) ].sort
    assert_equal [Arc.new(1,2), Arc.new(1,3), Arc.new(2,1), Arc.new(2,3)], x
  end

  def test_undirected_edge_new
    assert_raises(ArgumentError) {Edge.new}
    assert_raises(ArgumentError) {Edge.new(1)}
    assert Edge.new(1,2)
    assert Edge.new(1,2,'label')
  end

  def test_undirected_edge_getters
    assert_equal(1,@u.source)
    assert_equal(2,@u.target)
    assert_equal([1,2,'hoo'],@u.to_a)
    assert_equal("(1=2 'hoo')",@u.to_s)
  end

  def test_undirected_edge_methods
    @u.label = nil
    assert_equal("(1=2)",@u.to_s)
    assert_equal("(1=2)",Edge.new(2,1).to_s)

    assert @u.eql?(Edge.new(2,1))
    assert @u == Edge.new(2,1,'boo')
    assert @u != Edge.new(2,3)

    assert_equal(@u.hash,Edge.new(2,1).hash)
  end

  def test_undirected_edge_sort
    x=[Edge.new(12, 1), Edge.new(2,11)].sort
    assert_equal [Edge.new(2,11), Edge.new(1,12)], x
  end
  
  def test_hash
    assert_equal Arc[1,2,:b], Arc[1,2,:c]
    assert_equal Arc[1,2,:b].hash, Arc[1,2,:c].hash
    assert Arc[1,2] != Arc[2,1]
    assert Arc[1,2] != Edge[1,2]
    assert_equal Edge[1,2], Edge[2,1]
    assert_equal Edge[1,2,:a], Edge[2,1,:b]
  end

end

