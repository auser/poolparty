require "benchmark"
require "stream"
require "generator"

include Benchmark

a =(1..5000).to_a
s = a.create_stream
n = 10
g = Generator.new a

bm(15) do |test|
  test.report("A.each:") do
	n.times {a.each {|x| x}}
  end
  test.report("S.each:") do
	n.times {s.each {|x| x}}
  end
  test.report("G.each:") do
	n.times {g.each {|x| x}}
  end

  test.report("A.collect.first:") do
	n.times {a.collect {|x| x}.collect {|x| x}.first}
  end
  test.report("S.collect.first:") do
	n.times {s.collect {|x| x}.collect {|x| x}.first}
  end
  test.report("G.collect.first:") do
	n.times {g.collect {|x| x}.collect {|x| x}.first}
  end

  test.report("A.reverse:") do
	n.times {a.reverse.reverse.reverse.each {|x| x}}
  end
  test.report("S.reverse:") do
	n.times {s.reverse.reverse.reverse.each {|x| x}}
  end

  test.report("A.collect:") do
	n.times {a.collect {|x| x}.collect {|x| x}.each {|x| x}}
  end
  test.report("S.collect:") do
	n.times {s.collect {|x| x}.collect {|x| x}.each {|x| x}}
  end
  test.report("G.collect:") do
	n.times {g.collect {|x| x}.collect {|x| x}.each {|x| x}}
  end
end
