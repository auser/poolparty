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

  # === Description
  #
  # Change +attr+ to +val+ within the scope of block
  # and then sets it back again
  #
  def change_attr attr, val, &block
    old_val = instance_variable_get attr
    begin
      instance_variable_set attr, val
      yield
    ensure
      instance_variable_set attr, old_val
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
