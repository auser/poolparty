require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Host" do
  describe "instances" do
    before(:each) do
      @host = host({:name => "node1"})
    end
    it "should turn the one hash instance into a string" do
      @host.to_string.should =~ /node1/
    end
    describe "as included" do            
      before(:each) do
        @host = host do
          name "master"
          ip "192.168.0.1"
        end
      end
      it "should use default values" do
        @host.name.should == "master"
      end
      it "should also set options through a hash" do
        @host.ip.should == "192.168.0.1"
      end
    end
  end
end
