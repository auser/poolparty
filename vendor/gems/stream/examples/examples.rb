require 'stream'

# Example from knu's Generator.rb
g = ('a'..'f').create_stream
h = (1..10).create_stream
i = (10..20).create_stream

until g.at_end? || h.at_end? || i.at_end?
  p [g.forward, h.forward, i.forward]
end

puts "Concatenate Filestreams and collection stream:\n"

def fs fname
  Stream::ImplicitStream.new { |s|
	f = open(fname)
	s.at_end_proc = proc {f.eof?}
	s.forward_proc = proc {f.readline}
	# Need not implement backward moving to use the framework
  }
end

(fs("/etc/passwd") + ('a'..'f').create_stream + fs("/etc/group")).each do |l|
	puts l
end

puts "\nTwo filtered collection streams concatenated and reversed:\n\n"

def newstream; (1..6).create_stream; end
s = newstream.filtered { |x| x % 2 == 0 } \
		+ newstream.filtered { |x| x % 2 != 0 }
s = s.reverse
puts "Contents      : #{s.to_a.join ' '}"
puts "At end?       : #{s.at_end?}"
puts "At beginning? : #{s.at_beginning?}"
puts "2xBackwards   : #{s.backward} #{s.backward}"
puts "Forward       : #{s.forward}"
puts "Peek          : #{s.peek}"
puts "Current       : #{s.current}"
puts "set_to_begin    : Peek=#{s.set_to_begin;s.peek}"

# an infinite stream (do not use set_to_end!)
def randomStream
  Stream::ImplicitStream.new { |s|
	  s.set_to_begin_proc = proc {srand 1234}
	  s.at_end_proc = proc {false}
	  s.forward_proc = proc {rand}
	}
end
s = randomStream.filtered { |x| x >= 0.5 }.collect { |x| sprintf("%5.2f ",x*100) }
puts "5 random numbers: #{(1..5).collect {|x| s.forward}}\n"
