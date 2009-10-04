module GRATR
  class Digraph
    
    # Crappy n*n
    def find_cycle(from=self)
      return [] unless cyclic?
      cyclic_cycle = []
      forward_edge = Proc.new {|e| }
      back_edge    = Proc.new do |b| 
        cyclic_cycle = dfs_tree_from_vertex(b)
      end
      from.dfs({ 
       :forward_edge  => forward_edge,
       :back_edge    => back_edge
      })
      cyclic_cycle
    end
    
  end
end
