require 'rubygems'
require 'test/unit'
require 'shoulda'

$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))
$LOAD_PATH.unshift(File.dirname(__FILE__))
require 'baker'

class Test::Unit::TestCase
  def swallow_output(&block)
     old_stdout, old_stderr = $stdout, $stderr
     out = StringIO.new
     err = StringIO.new
     $stdout = out
     $stderr = err
     begin
        block.call if block
     ensure
        $stdout = old_stdout
        $stderr = old_stderr
     end
     out.string
  end  
end