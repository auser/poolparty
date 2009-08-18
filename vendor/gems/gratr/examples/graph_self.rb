#!/usr/bin/ruby -I../lib

require 'gratr/import'
require 'gratr/dot'

# This program gives an example of dynamic analysis of a program's call stack,
# that exports to dot and creates a jpg visualization of the call diagram.

class GraphSelf
  
  # Setup some data to call Dijkstra's Algorithm
  def initialize
    @d = Digraph[ [:a,:b] => 9, [:a,:e] => 3,
                  [:b,:c] => 2, [:b,:e] => 6,
                  [:c,:d] => 1,
                  [:d,:c] => 2,
                  [:e,:b] => 2, [:e,:f] => 1,
                  [:f,:c] => 2, [:f,:d] => 7, [:f,:e] => 2 ]

    @call_stack = []
    @call_graph = Digraph.new
  end
  
  # Get the call graph variable
  def call_graph() @call_graph; end
  
  # Turn capturing of call graph on
  def capture_func
    Proc.new do |event, f, l, id, b, klass|
      # Only interested in the GRATR library itself
      if ( klass.to_s =~ /GRATR/ ) 
        case event.to_s
          when /call/ 
            method = "#{klass.to_s.split('::')[1]}.#{id}" # Removes GRATR:: 
            @call_graph.add_edge!(@call_stack[-1],method) if @call_stack[-1] 
            @call_stack.push(method)
          when /return/ : @call_stack.pop
        end
      end
    end
  end 

  # Run a capture of the call graph for Dijkstra's algorithm  
  def run
    set_trace_func capture_func
    @d.dijkstras_algorithm(:a)
    set_trace_func nil
    self
  end
   
end

# Run a capture and generate the resulting jpg file
GraphSelf.new.run.call_graph.write_to_graphic_file('png','self_graph')