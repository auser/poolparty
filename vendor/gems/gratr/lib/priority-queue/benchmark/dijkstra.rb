$:.unshift "~/lib/ruby"
require 'priority_queue/ruby_priority_queue'
require 'priority_queue/poor_priority_queue'
require 'priority_queue/c_priority_queue'
require 'benchmark'

class Node
  attr_reader :neighbours, :id
  
  def initialize(id)
    @neighbours = []
    @id = id
  end
  
  def inspect
    to_s
  end

  def to_s
    "(#{@id})"
  end
end

# Build a graph by adding nodes with random connections

# Return a random graph with an average degree of degree
def make_graph(nodes, degree)
  nodes = Array.new(nodes) { | i | Node.new(i.to_s) }
  nodes.each do | n |
    (degree / 2).times do 
      true while (n1 = nodes[rand(nodes.length)]) == n
      n.neighbours << nodes[rand(nodes.length)]
      n1.neighbours << n
      n.neighbours << n1
    end
  end
end

def draw_graph(nodes, out)
  dot = [] << "graph g {"
  nodes.each do | n1 |
    dot << "N#{n1.id} [label='#{n1.id}'];"
    n1.neighbours.each do | n2 |
      dot << "N#{n1.id} -- N#{n2.id};" if n1.id <= n2.id
    end
  end
  dot << "}"

  #  system "echo '#{dot}' | neato -Gepsilon=0.001 -Goverlap=scale -Gsplines=true -Gsep=.4 -Tps -o #{out}"
  system "echo '#{dot}' | neato -Gepsilon=0.05 -Goverlap=scale -Gsep=.4 -Tps -o #{out}"
end

def dijkstra(start_node, queue_klass)
  # Priority Queue with unfinished nodes
  active = queue_klass.new
  # Distances for all nodes
  distances = Hash.new { 1.0 / 0.0 }
  # Parent pointers describing shortest paths for all nodes
  parents = Hash.new

  # Initialize with start node
  active[start_node] = 0
  until active.empty?
    u, distance = active.delete_min
    distances[u] = distance
    d = distance + 1
    u.neighbours.each do | v |
      next unless d < distances[v] # we can't relax this one
      active[v] = distances[v] = d
      parents[v] = u
    end    
  end
end

srand

sizes = Array.new(4) { | base | Array.new(9) { | mult | (mult+1) * 10**(base+2) } }.flatten
degrees = [2, 4, 16]
degrees = [4, 16]
degrees = [16]
queues = [  CPriorityQueue,  PoorPriorityQueue,  RubyPriorityQueue ]
queues = [  CPriorityQueue,  RubyPriorityQueue ]

max_time = 400
ignore = Hash.new

repeats = 5


STDOUT.sync = true

results = Hash.new { | h, k | h[k] = 
  Hash.new { | h1, k1 | h1[k1] = Hash.new { 0 } 
  } 
}

Benchmark.bm(30) do | b |
  sizes.each do | size |
    break if !ignore.empty? and ignore.values.inject(true) { | r, v | r and v }
    puts
    puts "Testing with graphs of size #{size}"
    degrees.each do | degree |
      repeats.times do | r |
	nodes = make_graph(size, degree)
	queues.each do | queue |      
	  next if ignore[queue]
	  GC.start
	  results[queue][degree][size] += (b.report("#{queue}: #{size} (#{degree})") do dijkstra(nodes[1], queue) end).real
	end
      end
      queues.each do | queue |      
	ignore[queue] ||= ((results[queue][degree][size] / repeats) > max_time)
      end
    end

    indices = queues.map { | q | degrees.map { | d | %&"#{q} (Graph of Degree: #{d})"& } }.flatten
    File.open("results.csv", "wb") do | f |
      f.puts "size\t" + indices.join("\t")
      sizes.each do | size |
	f.puts "#{size}\t" + queues.map { | q | degrees.map { | d | 
	  (results[q][d].has_key?(size) and results[q][d][size] > 0.0) ? results[q][d][size] / repeats : "''" 
	} }.join("\t")
      end
    end

    File.open("results.gp", 'wb') do | f |
      lines = []
      indices.each_with_index do | t, i |
	lines << "  'results.csv' using 1:#{i+2} with lines title #{t}"
      end
      f.puts "set term png"
      f.puts "set out 'results.png'"
      f.puts "set xlabel 'Number of nodes'"
      f.puts "set ylabel 'Time in seconds (real)'"
      f.puts "set logscale xy"
      f.puts "set title 'Dijkstras Shortest Path Algorithm using different PQ Implementations'"
      f.puts "plot \\"
      f.puts lines.join(",\\\n")
    end
      system "gnuplot results.gp"
    
    queues.each do | q | 
      File.open("result-#{q}.gp", 'wb') do | f |
	lines = []
	degrees.map { | d | %&"#{q} (Graph of Degree: #{d})"& }.flatten.each do | t |
	  lines << "  'results.csv' using 1:#{indices.index(t)+2} with lines title #{t}"
	end
	f.puts "set term png"
	f.puts "set out 'result-#{q}.png'"
	f.puts "set xlabel 'Number of nodes'"
	f.puts "set ylabel 'Time in seconds (real)'"
	f.puts "set logscale xy"
	f.puts "set title 'Dijkstras Shortest Path Algorithm on Networks of different degrees'"
	f.puts "plot \\"
	f.puts lines.join(",\\\n")
      end
      system "gnuplot result-#{q}.gp"
    end
  end
end

__END__

nodes = make_graph(100, 4)
draw_graph(nodes, "100-4.ps")
nodes = make_graph(100, 10)
draw_graph(nodes, "100-10.ps")
nodes = make_graph(10, 10)
draw_graph(nodes, "10-10.ps")
nodes = make_graph(1000, 2)
draw_graph(nodes, "1000-2.ps")
