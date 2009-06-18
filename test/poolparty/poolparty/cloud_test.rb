require "#{::File.dirname(__FILE__)}/../../test_helper"

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

    should "have access key on the cloud" do
      assert_equal "pool_access_key", pools[:test_pool_test].access_key
      assert_equal "pool_access_key", clouds[:orange].access_key
      assert_equal "cloud_access_key", clouds[:orange].secret_access_key
    end
    should "set the access_key even if the AWS_ACCESS_KEY is nil" do
      modify_env_with_hash("AWS_ACCESS_KEY" => nil)
      assert_equal "pool_access_key", clouds[:orange].access_key
    end
    should "set the secret_access_key even if the AWS_SECRET_ACCESS_KEY is nil" do
      modify_env_with_hash("AWS_SECRET_ACCESS_KEY" => nil)
      assert_equal "cloud_access_key", clouds[:orange].secret_access_key
    end
  end
  
  context "callbacks" do
    setup do
      cloud :apple do
        user "bob"
        minimum_instances 100        
        secret_access_key "cloud_access_key"
        
        after :bootstrap do |cld|
          cld.class.send :attr_reader, :after_bootstrap_message          
          @after_bootstrap_message = "After me!"
        end
      end
    end
    
    should "call have callbacks for after :bootstrap (defined inside the cloud)" do
      cblks = clouds[:apple].send :after_blocks
      assert_equal [:bootstrap], cblks.keys
      assert cblks[:bootstrap].is_a?(Array)
    end
    should "not have callbacks on configure (not defined inside the clouds.rb)" do
      cblks = clouds[:apple].send :before_blocks
      assert_equal [], cblks.keys
    end
    should "call the after bootstrap block when calling call_after_boostrap_callbacks on cloud" do
      apple = clouds[:apple]
      apple.call_after_bootstrap_callbacks
      assert_equal apple.after_bootstrap_message, "After me!"
    end
    should "not call after_configure blocks when calling call_after_configure_callbacks" do
      apple = clouds[:apple]
      apple.call_after_configure_callbacks
      assert_not_equal apple.after_bootstrap_message, "After me!"
    end
  end
end