require 'test/unit'
require 'generator2stream'

class TestGenerator < Test::Unit::TestCase
  include Stream

  def test_generator
	# Use WrappedStream to ensure that the stream protocol is used and
	# not the Enumerable protocol.
	g = Generator.new { |g|
	  for i in 1..3
		g.yield i
	  end
	  g.yield 0
	}
	assert_equal([1, 2, 3, 0], WrappedStream.new(g).to_a)
	
	# Generators are not reversable
	assert_raises(NotImplementedError) {g.reverse.first}

	a = (1..10).to_a
	assert_equal(a, WrappedStream.new(Generator.new(a)).to_a)
  end
end
