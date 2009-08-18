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

class TestDigraphDistance < Test::Unit::TestCase # :nodoc:

  def setup  
    @d = Digraph[ :a,:b, :a,:e,
                  :b,:c, :b,:e,
                  :c,:d,
                  :d,:c,
                  :e,:b, :e,:f,
                  :f,:c, :f,:d, :f,:e ]
  
    @w = {  Arc[:a,:b] => 9,
            Arc[:a,:e] => 3,
            Arc[:b,:c] => 2,
            Arc[:b,:e] => 6,
            Arc[:c,:d] => 1,
            Arc[:d,:c] => 2,
            Arc[:e,:b] => 2,
            Arc[:e,:f] => 1,
            Arc[:f,:c] => 2,
            Arc[:f,:d] => 7,
            Arc[:f,:e] => 2  }
    @a = {  :a => 0,
            :b => 5,
            :c => 6,
            :d => 7,
            :e => 3,
            :f => 4   }
    @simple_weight = Proc.new {|e| 1}            
  end
  
  def test_shortest_path
    x = Digraph[ :s,:u, :s,:w,
                 :j,:v,
                 :u,:j,
                 :v,:y,
                 :w,:u, :w,:v, :w,:y, :w,:x,
                 :x,:z ]
    assert x.acyclic?
    cost, path = x.shortest_path(:s,@simple_weight)
    assert_equal({:x=>2, :v=>2, :y=>2, :w=>1, :s=>0, :z=>3, :u=>1, :j=> 2}, cost)
    assert_equal({:x=>:w, :v=>:w, :y=>:w, :w=>:s, :z=>:x, :u=>:s, :j=>:u},  path)
  end
  
  def test_dijkstra_with_proc
    p = Proc.new {|e| @w[e]}
    distance, path = @d.dijkstras_algorithm(:a,p)
    assert_equal @a, distance
    assert_equal({ :d => :c,  :c => :f,  :f => :e,  :b => :e,  :e => :a}, path)
  end
  
  def test_dijkstra_with_label
    @w.keys.each {|e| @d[e] = @w[e]}
    assert_equal @a, @d.dijkstras_algorithm(:a)[0]
  end

  def test_dijkstra_with_bracket_label
    @w.keys.each do |e|
      @d[e] = { :xyz => (@w[e])} 
    end
    assert_equal @a, @d.dijkstras_algorithm(:a, :xyz)[0]
    @w.keys.each do |e|
      @d[e] = [@w[e]] 
    end
    assert_equal @a, @d.dijkstras_algorithm(:a, 0)[0]
  end
  
  def test_floyd_warshall
    simple = Digraph[ 0,1,  0,2,  1,2,  1,3,  2,3,  3,0 ]
    
    cost, path, delta = simple.floyd_warshall(@simple_weight)
    # Costs
    assert_equal({0=>3, 1=>1, 2=>1, 3=>2}, cost[0])
    assert_equal({0=>2, 1=>3, 2=>1, 3=>1}, cost[1])
    assert_equal({0=>2, 1=>3, 2=>3, 3=>1}, cost[2])
    assert_equal({0=>1, 1=>2, 2=>2, 3=>3}, cost[3])
    
    # Paths
    assert_equal({0=>1, 1=>1, 2=>2, 3=>1}, path[0])
    assert_equal({0=>3, 1=>3, 2=>2, 3=>3}, path[1])
    assert_equal({0=>3, 1=>3, 2=>3, 3=>3}, path[2])
    assert_equal({0=>0, 1=>0, 2=>0, 3=>0}, path[3])
    
    # Deltas
    assert_equal 1,  delta[0]
    assert_equal 1,  delta[1]
    assert_equal -1, delta[2]
    assert_equal -1, delta[3]
  end
  
  def test_bellman_ford_moore
    fig24 = Digraph[ [:s,:e] =>  8,
                     [:s,:d] =>  4,
                     [:e,:c] =>  2,
                     [:e,:d] => -5,
                     [:c,:b] => -2,
                     [:d,:c] => -2,
                     [:d,:a] =>  4,
                     [:a,:c] => 10,
                     [:a,:b] =>  9,
                     [:b,:c] =>  5,
                     [:b,:a] => -3]
    cost, path = fig24.bellman_ford_moore(:s)
    assert_equal({:e=>8, :d=>3, :c=>1, :b=>-1, :a=>-4, :s=>0}, cost)
    assert_equal({:e=>:s, :d=>:e, :c=>:d, :b=>:c, :a=>:b},     path)
  end
  
end