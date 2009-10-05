require 'rubygems'
require 'test/unit'
require "fileutils"

$LOAD_PATH.unshift(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))
require 'searchable_paths'

class Test::Unit::TestCase
end

class String
  def /(o)
    File.join(self, o.to_s)
  end
end