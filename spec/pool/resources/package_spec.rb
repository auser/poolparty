require File.dirname(__FILE__) + '/../../spec_helper'

describe "Package" do
  before(:each) do
    @package = PoolParty::Resources::Package.new
  end
  it "should have instances of files" do
    @package.respond_to?(:instances).should == true
  end
  describe "instances" do
    before(:each) do
      @instance1 = {:name => "puppetmaster"}
      @package << @instance1
    end
    it "should turn the one hash instance into a string" do
      @package.to_s.should == "package {\n\tpuppetmaster:\n\t\tname => puppetmaster;\n}"
    end
    it "should turn the two hash instance into a string" do
      @instance2 = {:name => "rails"}
      @package << @instance2
      @package.to_s.should == "package {\n\tpuppetmaster:\n\t\tname => puppetmaster;\n\trails:\n\t\tname => rails;\n}"
    end
  end
end
