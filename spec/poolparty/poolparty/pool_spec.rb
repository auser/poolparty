require File.dirname(__FILE__) + '/../spec_helper'

describe "Pool" do
  before(:each) do
    @obj = Object.new
  end
  it "should respond to the pool method" do
    @obj.respond_to?(:pool).should == true
  end
  it "should store the pool in the global array" do
    @p = pool :app do
    end
    @obj.pools[:app].should == @p
  end
    
  describe "block" do
    before(:each) do
      @pool = Pool.new(:test) do
        # Inside pool block
        Proc.new {puts "hello world"}
      end
    end
    
    it "should be able to define a cloud within the pool block" do
      @pool.respond_to?(:cloud).should == true
    end
    it "should evaluate the block when creating a new pool" do
      Proc.should_receive(:new).at_least(1)
      Pool.new(:test) do
        Proc.new {puts "hi"}
      end
    end
    describe "plugins" do
      after(:each) do
        @pool.instance_eval do
          plugin_directory "yaway"
        end
      end
      it "should call Dir when the plugin directory is set" do
        ::File.should_receive(:directory?).with("yaway").once.and_return true
        ::File.stub!(:directory?).and_return(false)
        Dir.should_receive(:[]).with("yaway/*/*.rb").once.and_return []        
      end
    end
    describe "configuration" do
      before(:each) do
        reset!
        @pool = Pool.new :test do
          expand_when "nails"
        end
      end
      it "should set the plugin_directory to nails" do
        @pool.expand_when.should == "nails"
      end
      describe "range for min/max instances" do
        it "should be able to respond to instances" do
          @pool.respond_to?(:instances).should == true
        end
        it "should be able to pass instances a range" do
          lambda {
            @pool.instance_eval do
              instances 2..5
            end
          }.should_not raise_error
        end
        it "should set the minimum as the minimum_instances from the range" do
          @pool.instance_eval do
            instances 2..5
          end
          @pool.minimum_instances.should == 2
        end
        it "should set the maximum as the maximum_instances from the range" do
          @pool.instance_eval do
            instances 2..5
          end
          @pool.maximum_instances.should == 5
        end
      end
    end  
    describe "with clouds" do
      before(:each) do
        reset!
        @pool = Pool.new :test do
          cloud :app
          cloud :db
        end
      end
      it "should have 2 clouds in the pool when there are 2 described" do
        @pool.clouds.size.should == 2
      end
      it "should be able to tel the other clouds from within one cloud" do
        cloud(:app).other_clouds.should == [cloud(:db)]
      end
    end  
  end
end