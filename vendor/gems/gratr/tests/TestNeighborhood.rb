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

class TestNeighborhood < Test::Unit::TestCase # :nodoc:
  
  def setup
    @d = Digraph[:a,:b, :a,:f,
                 :b,:g,
                 :c,:b, :c,:g,
                 :d,:c, :d,:g,
                 :e,:d,
                 :f,:e, :f,:g,
                 :g,:a, :g,:e]
    @w = [:a,:b]
  end
  
  def test_open_out_neighborhood
    assert_equal [:g], @d.set_neighborhood([:a],    :in)
    assert_equal [],   [:f,:g]  - @d.set_neighborhood(@w, :out)
    assert_equal [],   @w       - @d.open_pth_neighborhood(@w, 0, :out)
    assert_equal [],   [:f, :g] - @d.open_pth_neighborhood(@w, 1, :out)
    assert_equal [],   [:e]     - @d.open_pth_neighborhood(@w, 2, :out)
    assert_equal [],   [:d]     - @d.open_pth_neighborhood(@w, 3, :out)
    assert_equal [],   [:c]     - @d.open_pth_neighborhood(@w, 4, :out)    
  end
  
  def test_closed_out_neighborhood
    assert_equal [],   @w                     - @d.closed_pth_neighborhood(@w, 0, :out)
    assert_equal [],   [:a,:b,:f,:g]          - @d.closed_pth_neighborhood(@w, 1, :out)
    assert_equal [],   [:a,:b,:e,:f,:g]       - @d.closed_pth_neighborhood(@w, 2, :out)
    assert_equal [],   [:a,:b,:d,:e,:f,:g]    - @d.closed_pth_neighborhood(@w, 3, :out)
    assert_equal [],   [:a,:b,:c,:d,:e,:f,:g] - @d.closed_pth_neighborhood(@w, 4, :out)    
  end
  
  
end