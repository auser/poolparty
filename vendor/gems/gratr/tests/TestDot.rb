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

class TestDot < Test::Unit::TestCase # :nodoc:
  
  DG_DOT = <<-DOT
digraph  {
    label = "Datacenters"
    Miranda [
        color = green,
        style = filled,
        label = "Miranda"
    ]

    Miranda -> Hillview [

    ]

    Miranda -> "San Francisco" [

    ]

    Miranda -> "San Jose" [

    ]

    Sunnyvale -> Miranda [

    ]

}
DOT
  
  def setup
    @dg = DOT::DOTDigraph.new('label' => 'Datacenters')
    @dg << DOT::DOTNode.new('name' => 'Miranda', 'color' => 'green', 'style' => 'filled')
    @dg << DOT::DOTDirectedArc.new('from' => 'Miranda', 'to' => 'Hillview')
    @dg << DOT::DOTDirectedArc.new('from' => 'Miranda', 'to' => '"San Francisco"')
    @dg << DOT::DOTDirectedArc.new('from' => 'Miranda', 'to' => '"San Jose"')
    @dg << DOT::DOTDirectedArc.new('from' => 'Sunnyvale', 'to' => 'Miranda')
  end
  
  def test_generation
    assert @dg.to_s
    assert_equal DG_DOT, @dg.to_s
  end
  
end