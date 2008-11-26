require "rubygems"
require "poolparty"
Dir["#{::File.dirname(__FILE__)}/../core/*.rb"].each {|f| require f}

write_dynamic_matchers if @ensure_matchers_exist

Dir["#{::File.dirname(__FILE__)}/../matchers/*.rb"].each {|f| require f}