require "#{::File.dirname(__FILE__)}/../../../../test_helper"

# Uncomment the tests for live-testing
class TestMetavirt < Test::Unit::TestCase
  context "registered as remote base" do
    setup do
      reset!
      @cloud = cloud :metvirt_cld do
        keypair "#{::File.dirname(__FILE__)}/../../../../fixtures/test_key"
        instances 1
        using :metavirt do
          using :vmrun do
            vmx_files ['/path/to/vmx', '/another/path']
          end
        end
      end
    end
    should "be registered as a remote_base" do
      PoolParty::Remote::RemoterBase.available_bases.include?(:vmrun).should == true
    end
    should "be setting the type of remote_base" do
      assert_equal PoolParty::Remote::Vmrun, @cloud.remote_base.remote_base.class
      assert_equal PoolParty::Remote::Metavirt, @cloud.remote_base.class
    end
    should "have metavirt as the remoter base" do
      @cloud.metavirt.should == @cloud.remote_base
    end
    should "have a keypair" do
      assert_equal 'test_key', @cloud.keypair.basename
      assert_equal 'test_key', @cloud.keypair_name
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
        using :metavirt do
          using :libvirt do
            image_id 'jaunty19'
          end
        end
      end
    end
    
    should "be setting the type of remote_base" do
      assert_equal PoolParty::Remote::Libvirt, @cloud_lv.remote_base.remote_base.class
      assert_equal PoolParty::Remote::Metavirt, @cloud_lv.remote_base.class
    end
    
    should "be able to describe instances" do
      if ENV['LIBVIRT']  # set env variable if you want to actually call out to libvirt
        assert_equal 'jaunty19', @cloud_lv.metavirt.libvirt.image_id
        assert_equal 'jaunty19', @cloud_lv.metavirt.image_id
      end
    end
    
    should "use libvirt options from metavirt" do
      assert_equal 'jaunty19', clouds[:lv].metavirt.libvirt.image_id      
      assert_not_nil clouds[:lv].metavirt.image_id
      assert_equal clouds[:lv].metavirt.libvirt.image_id, clouds[:lv].metavirt.image_id
    end
  end
  
end