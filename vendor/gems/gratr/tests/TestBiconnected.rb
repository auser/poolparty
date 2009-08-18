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

class TestBiconnected < Test::Unit::TestCase # :nodoc:
  def test_tarjan
    tarjan = UndirectedGraph[ 1, 2,
                              1, 5,
                              1, 6, 
                              1, 7,
                              2, 3, 
                              2, 4,
                              3, 4,
                              2, 5,
                              5, 6,
                              7, 8,
                              7, 9,
                              8, 9 ]
    graphs, articulations = tarjan.biconnected
    assert_equal [1,2,7], articulations.sort
    assert_equal 4, graphs.size
    assert_equal [1,7],     graphs.find {|g| g.size == 2}.vertices.sort
    assert_equal [1,2,5,6], graphs.find {|g| g.size == 4}.vertices.sort
    assert_equal [2,3,4],   graphs.find {|g| g.size == 3 && g.vertex?(2)}.vertices.sort
    assert_equal [7,8,9],   graphs.find {|g| g.size == 3 && g.vertex?(7)}.vertices.sort
  end
end