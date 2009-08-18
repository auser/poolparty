#!/usr/bin/ruby -I../lib

require 'gratr/import'
require 'gratr/dot'

module_graph=Digraph.new
ObjectSpace.each_object(Module) do |m|
  m.ancestors.each {|a| module_graph.add_edge!(m,a) if m != a} 
end

gv = module_graph.vertices.select {|v| v.to_s.match(/GRATR/)}
module_graph.induced_subgraph(gv).write_to_graphic_file('jpg','module_graph')