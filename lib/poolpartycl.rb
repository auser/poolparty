Dir["#{File.dirname(__FILE__)}/poolparty/helpers/**.rb"].each do |lib|
  require lib
end

def help_array
  ["-h", "--help", "-V", "--version"]
end