class Object
    
  def pool(name=nil, &block)
    @@pool ||= PoolParty::Pool.new(name, &block)
  end
  
  def reset!
    @@pool = nil
  end
  
  def print_msg(msg_arr)
    msg_arr.each do |line|
      puts line
    end
  end
  
  def progress_bar_until(msg=nil, &block)
    print "#{msg}" if msg
    loop do
      if block.call
        break
      else
        $stdout.print "."
        $stdout.flush
        sleep 1
      end
    end
    print " OK" if msg
    puts "" if msg
  end
  
  
end