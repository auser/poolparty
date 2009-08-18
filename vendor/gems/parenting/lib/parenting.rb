$:.unshift(File.dirname(__FILE__)) unless
  $:.include?(File.dirname(__FILE__)) || $:.include?(File.expand_path(File.dirname(__FILE__)))

module Parenting
  VERSION = '0.0.4' unless const_defined?(:VERSION)
end

# Dir["parenting/core/*.rb"].each {|lib| require lib }

require "parenting/parenting"