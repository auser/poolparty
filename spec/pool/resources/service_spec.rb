require File.dirname(__FILE__) + '/../../spec_helper'

describe "Service" do
  before(:each) do
    @service = PoolParty::Resources::Service.new
  end
  it "should have instances of files" do
    @service.respond_to?(:instances).should == true
  end
  describe "instances" do
    before(:each) do
      @service << {:name => "puppetmaster"}
    end
    it "should turn the one hash instance into a string" do
      @service.to_s.should == "service {\n\tpuppetmaster:\n\t\tname => puppetmaster;\n}"
    end
    it "should turn the two hash instance into a string" do
      @service << {:name => "apache"}
      @service.to_s.should == "service {\n\tpuppetmaster:\n\t\tname => puppetmaster;\n\tapache:\n\t\tname => apache;\n}"
    end
  end
end
