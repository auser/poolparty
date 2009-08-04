# Let a Generator look like a Stream.
require 'stream'
require 'generator'

# The generator is made a Stream by aliasing
# the methods at_end?, basic_forward, basic_peek, set_to_begin to the approriate
# methods #end?, #next, #peek and #rewind of the Generator class.
#
# Be careful if you already use a version of Akinori MUSHAs generator.rb. Check out
# the version numbers of the one you use and the one comming with the stream package.
class Generator
  include Stream

  alias_method :at_end?, :end?
  alias_method :basic_forward, :next
  alias_method :basic_peek, :peek
  alias_method :set_to_begin, :rewind

  # Returns the generator itself.
  def create_stream; self; end
end
  
