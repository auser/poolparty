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

class TestMultiArc < Test::Unit::TestCase # :nodoc:

  def test_directed_pseudo_graph
    dpg=DirectedPseudoGraph[ :a,:b,
                             :a,:b,
                             :a,:b ]
    assert_equal 3, dpg.edges.size
    x=0
    dpg.edges.each {|e| dpg[e] = (x+=1)}
    assert_equal 6, dpg.edges.inject(0) {|a,v| a+=dpg[v]}
  end
  
  def test_directed_multi_graph
    dmg=DirectedMultiGraph[ :a,:a,
                            :a,:a,
                            :a,:b,
                            :a,:b,
                            :b,:b,
                            :b,:b ]
    assert_equal 6,  dmg.edges.size
    x = 0
    dmg.edges.each {|e| dmg[e] = (x+=1)}
    assert_equal 21, dmg.edges.inject(0) {|a,v| a+=dmg[v]}
  end
  
end