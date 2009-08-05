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

class TestInspection < Test::Unit::TestCase # :nodoc:
  
  def setup
    @dg = DirectedMultiGraph[ 
            [0,0,1]   => 1,
            [1,2,2]   => 2, 
            [1,3,3]   => 4,
            [1,4,4]   => nil, 
            [4,1,5]   => 8, 
            [1,2,6]   => 16, 
            [3,3,7]   => 32, 
            [3,3,8]   => 64     ]
    @dg[3] = 128
    @dg[0] = 256
  end
  
  def test_inspection
    inspect = @dg.inspect
    assert_equal 384, @dg.vertices.inject(0) {|a,v| a += (@dg[v] || 0)}
    assert_equal 127, @dg.edges.inject(0)    {|a,e| a += (@dg[e] || 0)}
    reflect = eval inspect
    assert reflect == @dg
    assert_equal 127, reflect.edges.inject(0)    {|a,e| a += (reflect[e] || 0)}
    assert_equal 384, reflect.vertices.inject(0) {|a,v| a += (reflect[v] || 0)}
  end
end