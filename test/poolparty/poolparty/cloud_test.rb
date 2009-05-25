require "#{::File.dirname(__FILE__)}/../../test_helper"

class CloudTest < Test::Unit::TestCase
  context "callbacks" do
    setup do
    reset!
      cloud :apple do
        user "bob"
        minimum_instances 100
        access_key "pool_access_key"
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