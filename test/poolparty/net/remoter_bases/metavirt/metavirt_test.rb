require "#{::File.dirname(__FILE__)}/../../../../test_helper"

# Uncomment the tests for live-testing
class TestMetavirt < Test::Unit::TestCase
  context "registered as remote base" do
    setup do
      reset!
      @cloud = cloud :test_metavirt_runner do
        keypair '~/.ec2/front'
        instances 1
        using :metavirt
      end
    end    
    should "be setting the type of remote_base as metavirt" do
      @cloud.remote_base.class.should == PoolParty::Remote::Metavirt
    end
    should "have the method metavirt as the remoter base" do
      @cloud.metavirt.should == @cloud.remote_base
    end
    should "have a keypair" do
      assert !@cloud.keypair.nil?
    end
    should "be able to initialize without a cloud" do
      mv = PoolParty::Remote::Metavirt.new
      mv.keypair.should == nil
      mv.authorized_keys.should == nil
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
  
end