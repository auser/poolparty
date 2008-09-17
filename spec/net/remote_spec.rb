require File.dirname(__FILE__) + '/../spec_helper'

module Hype
  def hyper
  end
  def instances_list
    []
  end
  register_remote_base :Hype
end
class TestClass
  include Remote
end

describe "Remote" do
  before(:each) do
    @tc = TestClass.new
  end
  it "should have the method 'using'" do
    @tc.respond_to?(:using).should == true
  end
  it "should include the module with using" do
    @tc.should_receive(:extend).with("Hype".preserved_module_constant).once
    @tc.using :hype
  end
  it "should keep a list of the remote_bases" do
    @tc.stub!(:remote_bases).and_return [:ec2, :hype]
    @tc.available_bases.should == [:ec2, :hype]
  end
  it "should be able to register a new base" do
    @tc.remote_bases.should_receive(:<<).with(:hockey).and_return true
    @tc.register_remote_base("Hockey")
  end
  it "should not extend the module if the remote base isn't found" do
    @tc.should_not_receive(:extend)
    hide_output do
      @tc.using :paper
    end    
  end
  it "should extend the module with RemoterBase" do
    Hype.should_receive(:extend).with(PoolParty::Remote::RemoterBase).once
    @tc.using :hype
  end
  describe "after using" do
    before(:each) do
      @tc = TestClass.new
      @tc.using :hype
    end
    it "should now have the methods available from the module" do
      @tc.respond_to?(:hyper).should == true
    end
    it "should raise an exception because the launch_new_instance! is not defined" do
      lambda {
        @tc.launch_new_instance!
      }.should raise_error
    end
    it "should not raise an exception because instances_list is defined" do
      lambda {
        @tc.instances_list
      }.should_not raise_error
    end
  end
end