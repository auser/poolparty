require "#{::File.dirname(__FILE__)}/../../../../test_helper"

# Uncomment the tests for live-testing
class TestLibvirt < Test::Unit::TestCase
  context "registered as remote base" do
    setup do
      reset!
      @lvcloud = cloud :vurt do
        keypair "#{::File.dirname(__FILE__)}/../../../../fixtures/test_key"
        instances 1
        using :libvirt do
        end
      end
    end
    should "be registered as a remote_base" do
      assert PoolParty::Remote.available.include?(PoolParty::Remote::Libvirt)
    end
    should "be setting the type of remote_base" do
      assert_equal PoolParty::Remote::Libvirt, @lvcloud.libvirt.class
    end
    should "have libvirt as the remoter base" do
      # assert_equal @lvcloud.libvirt.should, @lvcloud.remote_base
      assert_equal @lvcloud.remoter_base, :libvirt
      assert_equal @lvcloud.remote_base.class, PoolParty::Remote::Libvirt
    end
    should "have a keypair" do
      assert_equal 'test_key', @lvcloud.keypair.basename
      assert_equal 'test_key', @lvcloud.keypair_name
    end
    should "be able to initialize without a cloud" do
      assert_nothing_raised  do PoolParty::Remote::Libvirt.new end
    end
    # should "start metavirt instance" do
    #   @cloud.launch_instance!
    #   assert @cloud.metavirt.describe_instances.first[:instance_id]>0
    # end
    # should "be able to terminate the instance" do
    #   @cloud.launch_instance!
    #   @cloud.terminate_instance!
    #   assert @cloud.describe_instances.empty?
    # end
    # teardown do
    #   # PoolParty::Remote::metavirt.terminate!(
    #   @cloud.describe_instances.each do |inst|
    #     @cloud.terminate_instance! inst[:instance_id]
    #   end
    # end
  end
  
  context "running libvirt" do
    setup do
      @cloud_lv = cloud :lv do
        keypair "#{::File.dirname(__FILE__)}/../../../../fixtures/test_key"
        instances 1
        using :libvirt do
          image_id 'jaunty19'
        end
      end
    end
    should "be setting the type of remote_base" do
      assert_equal PoolParty::Remote::Libvirt, @cloud_lv.remote_base.class
      assert_equal 'jaunty19', @cloud_lv.remote_base.image_id
      assert_equal 'jaunty19', @cloud_lv.libvirt.image_id
    end
    should "be able to describe instances" do
      assert @cloud_lv.describe_instances.class == Array
    end
  end
  
end