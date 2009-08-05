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

class TestChinesePostman < Test::Unit::TestCase # :nodoc:

  def setup  
    @simple=Digraph[ 0,1,  0,2,  1,2,  1,3,  2,3,  3,0 ]
    @weight = Proc.new {|e| 1}
  end
  
  def test_closed_simple_tour
    tour = @simple.closed_chinese_postman_tour(0, @weight)
    assert_equal 11, tour.size
    assert_equal 0, tour[0]
    assert_equal 0, tour[10]
    edges = Set.new
    0.upto(9) do |n| 
      edges << Arc[tour[n],tour[n+1]]
      assert(@simple.edge?(tour[n],tour[n+1]), "Arc(#{tour[n]},#{tour[n+1]}) from tour not in graph")
    end
    assert_equal @simple.edges.size, edges.size, "Not all arcs traversed!"
  end
  
  
end