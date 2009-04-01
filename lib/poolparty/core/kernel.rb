=begin rdoc
  Kernel overloads
=end

module Kernel
  def get_latest_caller
    returning Array.new do |arr|
      callstack.size.times {|i| arr << callstack[i][0] unless callstack[i][0] =~ /lib\/poolparty/ }
    end.first
  end
  def callstack( level = 1 )
    call_str_array = caller(level)
    stack = []
    call_str_array.each{ |call_str|
      file, lineno, method = call_str.split(':')
      if method =~ /in `(.*)'/ then
        method = $1.intern()
      end
      stack << [file, lineno.to_i, method]
    }
    stack
  end
  # Nice wait instead of sleep
  def wait(time=5)
    sleep time.is_a?(String) ? eval(time) : time
  end
  def as(klass_or_obj, &block)
    block.in_context(klass_or_obj).call
  end
  def with_warnings_suppressed
    saved_verbosity = $-v
    $-v = nil
    yield
  ensure
    $-v = saved_verbosity
  end
  
  def capture_stdout(&block)
     old_stdout = $stdout
     out = StringIO.new
     $stdout = out
     begin
        block.call if block
     ensure
        $stdout = old_stdout
     end
     out.string
  end

  #redirect stdout and stderr to /dev/null and reopen after block
  def hide_output
    begin
      old_stdout = STDOUT.dup
      old_stderr = STDERR.dup
      STDOUT.reopen(File.open((PLATFORM =~ /mswin/ ? "NUL" : "/dev/null"), 'w'))
      STDERR.reopen(File.open((PLATFORM =~ /mswin/ ? "NUL" : "/dev/null"), 'w'))
      yield if block_given?
    ensure
      STDOUT.flush
      STDOUT.reopen(old_stdout)
      STDERR.flush
      STDERR.reopen(old_stderr)
    end
  end

end