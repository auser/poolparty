$:.unshift(File.dirname(__FILE__)) unless
  $:.include?(File.dirname(__FILE__)) || $:.include?(File.expand_path(File.dirname(__FILE__)))

require 'rubygems'
require 'test/unit'

require 'context/core_ext/string'
require 'context/core_ext/rails_hacks'

require 'context/version'
require 'context/suite'
require 'context/test'
require 'context/lifecycle'
require 'context/context'
require 'context/shared_behavior'

class Test::Unit::TestCase
  extend Context::Context
end
