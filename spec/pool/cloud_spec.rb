require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Cloud

describe "Cloud" do
  before(:each) do
    @obj = Object.new
    @pool = pool :just_pool do; end
  end
  it "should respond to the pool method outside the block" do
    @obj.respond_to?(:cloud).should == true
  end
  it "should store the cloud in the global list of clouds" do
    cloud :pop do;end
    @obj.clouds.has_key?(:pop).should == true
  end
  it "should return the cloud if the cloud key is already in the clouds list" do
    @cld = cloud :pop do;end
    @obj.cloud(:pop).should == @cld
  end
  describe "options" do
    before(:each) do
      @p = pool :options do
        minimum_instances 100
        cloud :apple do
          # minimum_instances 100
        end
      end
      @c = @p.cloud(:apple)
    end
    it "should be able to grab the cloud from the pool" do
      @c.should == @p.cloud(:apple)
    end
    it "should take the options set on the pool" do
      @p.minimum_instances.should == 100
    end
    it "should take the options set from the pool" do
      @c.minimum_instances.should == 100
    end
  end
  describe "block" do
    before(:each) do
      @cloud = Cloud.new(:test, @pool) do
        # Inside cloud block
      end
    end
    
    it "should be able to pull the pool from the cloud" do
      @cloud.parent == @pool
    end
    it "should respond to a configure method" do
      @cloud.respond_to?(:configure).should == true
    end
    describe "configuration" do
      before(:each) do
        @cloud2 = Cloud.new(:test, @pool) do
          minimum_instances 1
          maximum_instances 2
        end
      end
      it "should be able to se the minimum_instances without the var" do
        @cloud2.minimum_instances.should == 1
      end
      it "should be able to se the maximum_instances with the =" do
        @cloud2.maximum_instances.should == 2
      end
    end
    describe "options" do
      it "should set the minimum_instances to 2" do
        @cloud.minimum_instances.should == 2
      end
      it "should set the maximum_instances to 4" do
        @cloud.maximum_instances.should == 4
      end
      it "should be able to set the minimum instances" do
        @cloud.minimum_instances 3
        @cloud.minimum_instances.should == 3
      end
      it "should be able to take a hash from configure and convert it to the options" do
        @cloud.configure( {:minimum_instances => 1, :maximum_instances => 10, :keypair => "friend"} )
        @cloud.keypair.should == "friend"
      end
    end
    
  end
end