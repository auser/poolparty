$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))
$LOAD_PATH.unshift(File.dirname(__FILE__))

ENV['RACK_ENV'] ||= 'test'

# Test dependencies
%w(fakeweb right_http_connection matchy shoulda).each do |dep|
  $LOAD_PATH.unshift(File.join(File.dirname(__FILE__),'..', 'vendor/gems', dep, 'lib'))
  # require "#{dep}"
end

require 'poolparty'

require "rubygems"
require "test/unit"
# TODO: Rip out shoulda
require "shoulda"
require 'mocha' # ARG!  don't want to introduce additional test dependencies, but FakeWeb can only handle one fake post per URL!!!
require 'webmock/test_unit'
require 'pathname'

require 'git-style-binary/command'

require "test_methods"

GitStyleBinary.run = true

FIXTURES_PATH = Pathname.new(File.expand_path('../fixtures', __FILE__))