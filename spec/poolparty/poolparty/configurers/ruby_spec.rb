require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/../test_plugins/webserver'

describe "Configurer" do
  before(:each) do
    reset!
    $pool_specfile = @basic = File.join(File.dirname(__FILE__), "files", "ruby_basic.rb")
    PoolParty::Pool::Pool.load_from_file @basic
    @conf = Object.new
  end
  it "should not be nil" do
    @conf.should_not be_nil
  end
  
  describe "with a spec file" do
    describe "clouds" do
      before(:each) do
        reset!
        PoolParty::Pool::Pool.load_from_file @basic
        @cloud = clouds[:app]
      end
      it "should contain a list of the clouds within the pool (:app)" do
        @cloud.should_not be_nil
      end
      it "should set the minimum instances on the :app cloud" do
        @cloud.minimum_instances.should == 1
      end
      it "should set the maximum instances on the :app cloud" do
        @cloud.maximum_instances.should == 5
      end
      it "should set the keypair name on the :app cloud too" do
        @cloud.keypair.to_s.should =~ /id_rsa/
      end
    end
  end
  
end