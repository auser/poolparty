require File.dirname(__FILE__) + '/../spec_helper'

class ResourcerTestClass
  include CloudResourcer
  include Configurable
  
  # Stub keypair
  def keypair
    "rangerbob"
  end
end
class TestParentClass
  def options
    {}
  end
  def services
    @services ||= []
  end
  def add_service(s)
    services << s
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
  it "should provide set_parent" do
    @tc.respond_to?(:set_parent).should == true
  end
  describe "parents" do
    before(:each) do
      @testparent = TestParentClass.new
    end
    describe "setting" do
      it "should add the child to its services" do
        @testparent.should_receive(:add_service)
      end
      it "should call configure with options" do
        @tc.should_receive(:configure).with(@testparent.options)      
      end
      after do
        @tc.set_parent(@testparent)
      end      
    end
    describe "parent's services" do
      before(:each) do        
        @tc.set_parent(@testparent)        
      end
      it "should set the parent" do
        @tc.parent.should == @testparent
      end
      it "should have one service set" do
        @testparent.services.size.should == 1
      end
      it "should have the child in the parent's services" do
        @testparent.services.first.should == @tc
      end
    end
    
  end
end