$:.unshift(::File.dirname(__FILE__))
$:.unshift(::File.dirname(__FILE__) + "/../lib")
require 'poolparty'
require "rubygems"
require "test/unit"
# require "context"
require "matchy"
require "shoulda"
require "mocha"

require "test_methods"

require 'git-style-binary/command'

GitStyleBinary.run = true

modify_env_with_hash(
  "AWS_ACCESS_KEY" => "fake_aws_access_key", 
  "AWS_SECRET_ACCESS_KEY" => "fake_secret_aws_key"
  )
