require File.dirname(__FILE__) + '/../../spec_helper'

describe "Configuer" do
  before(:each) do
    @conf = Object.new
  end
  it "should not be nil" do
    @conf.should_not be_nil
  end
  
  describe "with a spec file" do
    before(:each) do
      @s = Script.new
      Script.stub!(:new).and_return(@s)
      @proc = Proc.new {  }
      @pool = Pool.new :test, &@proc
      @basic = read_file(File.join(File.dirname(__FILE__), "files", "ruby_basic.rb"))
    end
    it "should load the basic example configure" do
      @s.should_receive(:inflate).and_return true
    end
    it "should call inflate on the pools" do
      @s.pools.each {|a,b| b.should_receive(:inflate).and_return true }
    end
    describe "clouds" do
      before(:each) do
        @cloud = @s.pool(:poolpartyrb).cloud(:app)
      end
      it "should contain a list of the clouds within the pool (:app)" do
        @cloud.should_not be_nil
      end
      it "should set the minimum instances on the :app cloud" do
        @cloud.minimum_instances.should == 1
      end
      it "should set the maximum instances on the :app cloud" do
        @cloud.maximum_instances.should == 1
      end
      it "should set the keypair name on the :app cloud too" do
        @cloud.keypair.should == "name"
      end
    end
    
    after do
      Script.inflate @basic
    end
  end
  
end