$:.unshift(::File.dirname(__FILE__))
$:.unshift(::File.dirname(__FILE__) + "/../lib")

# Test dependencies
%w(fakeweb right_http_connection matchy shoulda).each do |dep|
  $LOAD_PATH.unshift(File.join(File.dirname(__FILE__),'..', 'vendor/gems', dep, 'lib'))
  # require "#{dep}"
end

require "test_methods"

modify_env_with_hash(
  "EC2_ACCESS_KEY" => "fake_access_key", 
  "EC2_SECRET_KEY" => "fake_secret_key",
  "EC2_PRIVATE_KEY" => ::File.dirname(__FILE__) + "/fixtures/keys/test_key",
  "EC2_CERT"        => ::File.dirname(__FILE__) + "/fixtures/keys/test_key",
  "EC2_USER_ID"     => '1234567890'
  )

require 'poolparty'
require "rubygems"
require "test/unit"
# TODO: Rip out shoulda and matchy
require "matchy"
require "shoulda"
 
require 'git-style-binary/command'

GitStyleBinary.run = true

