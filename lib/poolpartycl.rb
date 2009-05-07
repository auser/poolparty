Dir["#{File.dirname(__FILE__)}/poolparty/helpers/**.rb"].each do |lib|
  require lib
end

def help_array
  ["-h", "--help", "-V", "--version", "--debug", "-d"]
end

def are_you_sure?(msg)
  puts msg
  resp = gets.strip!

  case resp
  when "Y"
  when "yes"
  when "y"
    return true
  else
    return false
  end
end