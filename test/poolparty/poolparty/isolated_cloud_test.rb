require "rubygems"
require "test/unit"
require "matchy"
require "shoulda"
require File.dirname(__FILE__) + '/../../../lib/poolparty'

class CloudTest < Test::Unit::TestCase
  context "pulling options" do
    setup do
      reset!      
      pool :test_pool_test do
        access_key "pool_access_key"
        cloud :orange do
          secret_access_key "cloud_access_key"
        end
      end
    end
    should "set the access_keys" do
      assert_equal "pool_access_key", pools[:test_pool_test].access_key
      assert_equal nil, pools[:test_pool_test].secret_access_key
      assert_equal "pool_access_key", clouds[:orange].access_key
      assert_equal "cloud_access_key", clouds[:orange].secret_access_key
    end
  end
end