require File.dirname(__FILE__) + '/../../spec_helper'

describe "File" do
  before(:each) do
    @file = PoolParty::Resources::File.new
  end
  it "should have instances of files" do
    @file.respond_to?(:instances).should == true
  end
  describe "instances" do
    before(:each) do
      @instance1 = {:name => "/etc/apache2/puppetmaster.conf"}
      @file << @instance1
    end
    it "should turn the one hash instance into a string" do
      @file.to_s.should == "file {\n\tpuppetmaster.conf:\n\t\tname => /etc/apache2/puppetmaster.conf;\n}"
      
    end
    it "should turn the two hash instance into a string" do
      @instance2 = {:name => "/etc/init.d/puppetmaster"}
      @file << @instance2
      @file.to_s.should == "file {\n\tpuppetmaster.conf:\n\t\tname => /etc/apache2/puppetmaster.conf;\n\tpuppetmaster:\n\t\tname => /etc/init.d/puppetmaster;\n}"
      
    end
  end
end
