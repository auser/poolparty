require "rubygems"
require "spec"

$:.unshift(File.dirname(__FILE__) + '/../../lib')

require "#{::File.dirname(__FILE__)}/../../vendor/gems/dslify/lib/dslify"
require "poolparty/poolparty/default"
require "poolparty/schema"