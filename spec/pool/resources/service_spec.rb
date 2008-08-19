require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Service" do
  before(:each) do
    @service = PoolParty::Resources::Service.new
  end
  it "should have instances of Services" do
    @service.respond_to?(:instances).should == true
  end
  describe "instances" do
    before(:each) do
      @instance1 = {:name => "/etc/apache2/puppetmaster.conf"}
      @service << @instance1
    end
    it "should turn the one hash instance into a string" do
      @service.to_s.should =~ /name => \/etc\/apache2\/puppetmaster\.conf;/      
    end
    it "should turn the two hash instance into a string" do
      @instance2 = {:name => "/etc/init.d/puppetmaster"}
      @service << @instance2
      @service.to_s.should =~ /name => \/etc\/apache2\/puppetmaster\.conf;/
    end
    describe "as included" do            
      before(:each) do
        @service = service({:rent => "low", :ensure => "running"}) do
          name "mdmdp"
          besure "running"
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
        @service.ensure.should == "running"
      end
    end
  end
end
