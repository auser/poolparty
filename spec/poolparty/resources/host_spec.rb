require File.dirname(__FILE__) + '/../spec_helper'

describe "Host" do
  describe "instances" do
    before(:each) do
      @tc = TestBaseClass.new do
        has_host({:name => "node10"}) do
          aka "backup_mysql_master"
          ip "10.0.0.1"
        end
      end
      @host = @tc.resource(:host).first
    end
    it "have the name in the options" do
      @host.name.should == "node10"
    end
    it "should store the mode (from within the block)" do
      @host.ip.should == "10.0.0.1"
    end
    describe "into PuppetResolver" do
      before(:each) do
        @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
      end
      it "should set the name to the name of the host" do
        @compiled.should match(/host \{ "node10"/)
      end
      it "set the ip to the ip" do
        @compiled.should match(/ip => "10.0.0.1"/)
      end
      it "should say it's alias is set" do
        @compiled.should match(/alias => "backup_mysql_master"/)
      end
    end
  end
end
