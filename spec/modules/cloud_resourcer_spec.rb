require File.dirname(__FILE__) + '/../spec_helper'

class ResourcerTestClass
  include CloudResourcer
  include Configurable
  
  # Stub keypair
  def keypair
    "rangerbob"
  end
end
describe "CloudResourcer" do
  before(:each) do
    @tc = ResourcerTestClass.new
  end
  it "should have the method instances" do
    @tc.respond_to?(:instances).should == true
  end
  it "should be able to accept a range and set the first to the minimum instances" do
    @tc.instances 4..10
    @tc.minimum_instances.should == 4
  end
  it "should set the max to the maximum instances to the last" do
    @tc.instances 4..10
    @tc.maximum_instances.should == 10
  end
  describe "keypair_path" do
    it "should have the keypair_path" do
      @tc.respond_to?(:keypair_path).should == true
    end
    it "should set the keypair to the Base.keypair_path" do      
      @tc.keypair_path.should =~ /#{Base.base_keypair_path}/
    end
    it "should set the keypair to have the keypair set" do
      @tc.keypair.should =~ /rangerbob/
    end
    it "should set it to the Base keypair_path and the keypair" do
      @tc.keypair_path.should == "#{Base.base_keypair_path}/#{@tc.keypair}"
      @tc.keypair_path.should == "~/.ec2/rangerbob"
    end
  end
end