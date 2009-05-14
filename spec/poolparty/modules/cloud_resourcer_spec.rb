require File.dirname(__FILE__) + '/../spec_helper'

class ResourcerTestClass < PoolParty::Cloud::Cloud  
  default_options({
    :minimum_runtime => 50.minutes
  })
end
class TestParentClass < PoolParty::Cloud::Cloud  
  # def services
  #   @services ||= []
  # end
  # def add_service(s)
  #   services << s
  # end
end
describe "CloudResourcer" do
  before(:each) do
    @tc = Cloud.new :bank do
    end
  end
  it "should have the method instances" do
    @tc.respond_to?(:instances).should == true
  end
  it "should be able to accept a range and set the first to the minimum instances" do
    @tc.instances 4..10
    @tc.minimum_instances.should == 4
  end
  it "should be able to accept a Fixnum and set the minimum_instances and maximum_instances" do
    @tc.instances 1
    @tc.minimum_instances.should == 1
    @tc.maximum_instances.should == 1
  end
  it "should set the max to the maximum instances to the last in a given range" do
    @tc.instances 4..10
    @tc.maximum_instances.should == 10
  end
  it "should have default minimum_runtime of 50 minutes (3000 seconds)" do
    Default.stub!(:minimum_runtime).and_return 50.minutes
    @tc.minimum_runtime.should ==  50.minutes
  end
  it "should have minimum_runtime" do
    @tc.minimum_runtime 40.minutes
    @tc.minimum_runtime.should == 40.minutes
  end
  describe "parents" do
    before(:each) do
      @testparent = 
      TestParentClass.new(:parent_of_bob) do
        test_option "blankity blank blank"
        
        ResourcerTestClass.new :bob do
        end
      end
      describe "setting" do
        it "set 1 service on the parent class" do
          @testparent.services.size.should == 1
        end
        it "set the service as a ResourcerTestClass named bob" do
          @testparent.services.first.name.should == :bob
        end
        it "set the parent's options on the child" do
          @testparent.services.first.test_option.should == "blankity blank blank"
        end
      end
    end
  end
end