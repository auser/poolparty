require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Service" do
  before(:each) do
    reset_resources!
    @service = PoolParty::Resources::Service.new
  end
  describe "instances" do
    before(:each) do
      @service = service({:name => "/etc/apache2/puppetmaster.conf"})
    end
    it "should turn the one hash instance into a string" do
      @service.to_string.should =~ /\/etc\/apache2\/puppetmaster\.conf:/
    end
    it "should turn the two hash instance into a string" do
      @service = service({:name => "/etc/init.d/puppetmaster"})
      @service.to_string.should =~ /\/etc\/init\.d\/puppetmaster:/
    end
    describe "as included" do            
      before(:each) do
        reset_resources!
        @service = service({:rent => "low", :ensure => "stopped"}) do
          name "mdmdp"
        end
      end
      it "should use default values" do
        @service.name.should == "mdmdp"
      end
      it "should keep the default values for the Service" do
        @service.enable.should == "true"
      end
      it "should also set options through a hash" do
        @service.rent.should == "low"
      end
      it "should also set options through a hash" do
        @service.ensure.should == "stopped"
      end
    end
  end
end
