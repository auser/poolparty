=begin rdoc
  Binaries helper moduel
=end
module Printing
  # print_msg
  # Params
  #   msg_array - Array of strings
  # Usage:
  #   msg = ["hello", "world"]
  #   print_msg(msg)
  def print_msg(msg_array)
    arr = msg_array.map {|line| Colors.process(line) }
    max_size = arr.sort {|a,b| a.size <=> b.size }.last.size
    arr.each do |line|
      puts line.ljust(max_size)
    end
    Colors.reset!
  end
  
  # Ask for response
  def are_you_sure?(msg)
    puts msg
    resp = STDIN.gets.chomp

    case resp
    when "Y"
    when "yes"
    when "y"
      return true
    else
      return false
    end
  end
  
end