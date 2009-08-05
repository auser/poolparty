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

class TestSearch < Test::Unit::TestCase # :nodoc:

  def setup
    @directed   = Digraph[1,2, 2,3, 2,4, 4,5, 1,6, 6,4]
    @undirected = UndirectedGraph[1,2, 2,3, 2,4, 4,5, 1,6]
    @tree       = Digraph[ 1,2, 1,3, 1,4, 2,5, 2,4, 2,6, 6,7, 23,24 ]
  end

  # "Algorithmic Graph Theory and Perfect Graphs", Martin Charles
  # Golumbic, 1980, Academic Press, page 38, Figure 2.6
  def assign_dfsnumber_ancestry(graph, dfsnumber, father, start)
    i = 0
    dfsnumber.clear
    father.clear
    ev = Proc.new {|v| dfsnumber[v]     = (i+=1)   }
    te = Proc.new {|e| father[e.target] = e.source }
    graph.dfs({:enter_vertex => ev, :tree_edge => te, :start => start})
  end

  # Is v an ancestor of u
  def ancestor?(father, u, v)
    i = 1
    while v
      return i if father[v] == u
      v = father[v]
      i += 1
    end; nil
  end

  # Is there any relationship?
  def related?(father,u,v) ancestor?(father,u,v) or ancestor?(father,v,u); end

  # "Algorithmic Graph Theory and Perfect Graphs", Martin Charles
  # Golumbic, 1980, Academic Press, page 39, Propery (D1) and (D2) of 
  # depth first search
  def test_dfs_properties
    dfs = {}
    father = {}
    @directed.each do |vertex|
      assign_dfsnumber_ancestry(@directed, dfs, father, vertex)  
      # Property (D1)
      father.keys.each {|v| assert(dfs[father[v]] < dfs[v])}
      # Property (D2)
      # FIXME: Huh? Doesn't work
      #@directed.edges.each {|e| assert(related?(father, e.source, e.target))}
      #@directed.edges.each {|e| assert(dfs[e.source] < dfs[e.target])}
    end
    assert_equal 6, @directed.dfs.size
    assert_equal @directed.vertices.sort, @directed.dfs.sort
  end

  # "Algorithmic Graph Theory and Perfect Graphs", Martin Charles
  # Golumbic, 1980, Academic Press, page 40, Figure 2.7 
  def assign_bfsnumber_ancestry(graph, bfsnum, level, father, start)
    i = 0
    bfsnum.clear
    level.clear
    father.clear
    rt = Proc.new {|v| level[v] = 0 }
    ev = Proc.new {|v| bfsnum[v]=(i+=1);level[v]=(level[father[v]]+1) if father[v]}
    te = Proc.new {|e| father[e.target] = e.source }
    graph.dfs({:enter_vertex => ev, :tree_edge => te, 
               :root_vertex => rt, :start => start})
  end

  # "Algorithmic Graph Theory and Perfect Graphs", Martin Charles
  # Golumbic, 1980, Academic Press, page 40, Propery (B1), (B2) and (B3) of 
  # breadth first search
  def test_bfs_properties
    level  = {}  
    father = {}
    bfs    = {}
    @directed.each do |vertex|
      assign_bfsnumber_ancestry(@directed, bfs, level, father, vertex)  
      # Property (B1)
      father.keys.each {|v| assert(bfs[father[v]] < bfs[v])}
      # Property (B2)
      @directed.edges.each {|e| assert((level[e.source]-level[e.target]).abs<2)}
      # Property (B3)
      # FIXME: How can one test this?
      #@directed.vertex.each {|v| assert((level[e.source]-level[e.target]).abs<2)}
    end
    assert_equal 6, @directed.dfs.size
    assert_equal @directed.vertices.sort, @directed.dfs.sort
  end

  def test_cyclic
    assert @directed.acyclic?
    assert @undirected.acyclic?
    assert !@directed.cyclic?
    assert !@undirected.cyclic?
    @undirected.add_edge!(4,6)
    @directed.add_edge!(3,1)
    assert !@directed.acyclic?
    assert !@undirected.acyclic?
    assert @directed.cyclic?
    assert @undirected.cyclic?

    # Test empty graph
    x = Digraph.new
    assert !x.cyclic?
    assert x.acyclic?
  end

  def test_astar
    # Graph from "Artificial Intelligence: A Modern Approach" by Stuart
    # Russell ande Peter Norvig, Prentice-Hall 2nd Edition, pg 63
    romania = UndirectedGraph.new.
      add_edge!('Oradea',         'Zerind',          71).
      add_edge!('Oradea',         'Sibiu',          151).
      add_edge!('Zerind',         'Arad',            75).
      add_edge!('Arad',           'Sibiu',           99).
      add_edge!('Arad',           'Timisoara',      138).
      add_edge!('Timisoara',      'Lugoj',          111).
      add_edge!('Lugoj',          'Mehadia',         70).
      add_edge!('Mehadia',        'Dobreta',         75).
      add_edge!('Dobreta',        'Craiova',        120).
      add_edge!('Sibiu',          'Fagaras',         99).
      add_edge!('Fagaras',        'Bucharest',      211).
      add_edge!('Sibiu',          'Rimnicu Vilcea',  80).
      add_edge!('Rimnicu Vilcea', 'Craiova',        146).
      add_edge!('Rimnicu Vilcea', 'Pitesti',         97).
      add_edge!('Craiova',        'Pitesti',        138).
      add_edge!('Pitesti',        'Bucharest',      101).
      add_edge!('Bucharest',      'Giurgin',         90).
      add_edge!('Bucharest',      'Urzieni',         85).
      add_edge!('Urzieni',        'Hirsova',         98).
      add_edge!('Urzieni',        'Vaslui',         142).
      add_edge!('Hirsova',        'Eforie',          86).
      add_edge!('Vaslui',         'Iasi',            92).
      add_edge!('Iasi',           'Neamt',           87)

    # Heuristic from "Artificial Intelligence: A Modern Approach" by Stuart
    # Russell ande Peter Norvig, Prentice-Hall 2nd Edition, pg 95
    straight_line_to_Bucharest = 
    {
      'Arad'           => 366,
      'Bucharest'      =>   0,
      'Craiova'        => 160,
      'Dobreta'        => 242,
      'Eforie'         => 161,
      'Fagaras'        => 176,
      'Giurgiu'        =>  77,
      'Hirsova'        => 151,
      'Iasi'           => 226,
      'Lugoj'          => 244,
      'Mehadia'        => 241,
      'Neamt'          => 234,
      'Oradea'         => 380,
      'Pitesti'        => 100,
      'Rimnicu Vilcea' => 193,
      'Sibiu'          => 253,
      'Timisoara'      => 329,
      'Urziceni'       =>  80,
      'Vaslui'         => 199,
      'Zerind'         => 374
    }

    # Heuristic is distance as crow flies, always under estimates costs.
    h   = Proc.new {|v| straight_line_to_Bucharest[v]}

    list = []

    dv  = Proc.new {|v| list << "dv #{v}" }
    ev  = Proc.new {|v| list << "ev #{v}" }
    bt  = Proc.new {|v| list << "bt #{v}" }
    fv  = Proc.new {|v| list << "fv #{v}" }
    er  = Proc.new {|e| list << "er #{e}" }
    enr = Proc.new {|e| list << "enr #{e}" }
 
    options = { :discover_vertex  => dv,
                :examine_vertex   => ev,
                :black_target     => bt,
                :finish_vertex    => fv,
                :edge_relaxed     => er,
                :edge_not_relaxed => enr }
                      
    result = romania.astar('Arad', 'Bucharest', h, options)

    assert_equal ["Arad", "Sibiu", "Rimnicu Vilcea", "Pitesti", "Bucharest"], result
    # This isn't the greatest test since the exact ordering is not
    # not specified by the algorithm. If someone has a better idea, please fix
    assert_equal ["ev Arad",
     "er (Arad=Sibiu '99')",
     "dv Sibiu",
     "er (Arad=Timisoara '138')",
     "dv Timisoara",
     "er (Arad=Zerind '75')",
     "dv Zerind",
     "fv Arad",
     "ev Sibiu",
     "er (Rimnicu Vilcea=Sibiu '80')",
     "dv Rimnicu Vilcea",
     "er (Fagaras=Sibiu '99')",
     "dv Fagaras",
     "er (Oradea=Sibiu '151')",
     "dv Oradea",
     "enr (Arad=Sibiu '99')",
     "fv Sibiu",
     "ev Rimnicu Vilcea",
     "enr (Rimnicu Vilcea=Sibiu '80')",
     "er (Craiova=Rimnicu Vilcea '146')",
     "dv Craiova",
     "er (Pitesti=Rimnicu Vilcea '97')",
     "dv Pitesti",
     "fv Rimnicu Vilcea",
     "ev Fagaras",
     "enr (Fagaras=Sibiu '99')",
     "er (Bucharest=Fagaras '211')",
     "dv Bucharest",
     "fv Fagaras",
     "ev Pitesti",
     "enr (Pitesti=Rimnicu Vilcea '97')",
     "er (Bucharest=Pitesti '101')",
     "enr (Craiova=Pitesti '138')",
     "fv Pitesti",
     "ev Bucharest"], list
  end
  
  def test_bfs_spanning_forest
    predecessor, roots = @tree.bfs_spanning_forest(1)
    assert_equal({2=>1, 3=>1, 4=>1, 5=>2, 6=>2, 7=>6, 24=>23}, predecessor)
    assert_equal [1,23], roots.sort
    predecessor, roots = @tree.bfs_spanning_forest(3)
    assert_equal({7=>6, 24=>23, 2=>1, 4=>1}, predecessor)
    assert_equal [1,3,5,6,23], roots.sort    
  end
  
  def test_dfs_spanning_forest
    predecessor, roots = @tree.dfs_spanning_forest(1)
    assert_equal({5=>2, 6=>2, 7=>6, 24=>23, 2=>1, 3=>1, 4=>2}, predecessor)
    assert_equal [1,23], roots.sort
    predecessor, roots = @tree.dfs_spanning_forest(3)
    assert_equal({7=>6, 24=>23, 2=>1, 4=>2}, predecessor)
    assert_equal [1,3,5,6,23], roots.sort    
  end
  
  def test_tree_from_vertex
    assert_equal({5=>2, 6=>2, 7=>6, 2=>1, 3=>1, 4=>1}, @tree.bfs_tree_from_vertex(1))
    assert_equal({}, @tree.bfs_tree_from_vertex(3))
    assert_equal({5=>2, 6=>2, 7=>6, 2=>1, 3=>1, 4=>2}, @tree.dfs_tree_from_vertex(1))
    assert_equal({}, @tree.dfs_tree_from_vertex(3))
  end

end
