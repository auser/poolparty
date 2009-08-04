#
# Simple stream test interface based on Ruby-FLTK
# See: (http://ruby-fltk.sourceforge.net/)
#
require 'fltk'
require 'stream'

class StreamTester < Fltk::Window
  include Stream
  def initialize (s)
	super(200,200)
	@stream = s

	pack = Fltk::Pack.new(200,200) {
	  %w{set_to_begin set_to_end forward backward at_end? at_beginning? current_edge entries inspect}.each { |m|
		Fltk::Button.new(50,20,m) {|w,data|
		  @output.value(sendMsg(m))
		  puts sendMsg(m)
		}
	  }
	  @output= Fltk::Input.new(200, 20)
	  @output.deactivate
	}
	pack.packtype = Fltk::VERTICAL
	self.add(pack)
	self.show
  end

  def sendMsg(m)
	begin
	  @stream.send(m).inspect
	rescue EndOfStreamException => msg
	  msg
	end
  end
end

module Stream
  def openTester
	StreamTester.new(self)
	Fltk::run()
  end
end

if $0 == __FILE__
  def newstream; (1..6).create_stream; end
  s = newstream.filtered { |x| x % 2 == 0 } \
		+ newstream.filtered { |x| x % 2 != 0 }
  s.reverse.openTester

  # an infinite stream (do not use set_to_end!)
  randomStream =
	Stream::ImplicitStream.new { |s|
	s.set_to_begin_proc = proc {srand 1234}
	s.at_end_proc = proc {false}
	s.forward_proc = proc {rand}
  }
  randomStream.filtered { |x| x >= 0.5 }.collect { |x| x*100 }.openTester
end
