require File.dirname(__FILE__) + '/spec_helper'

include PoolParty
describe "Dependencies" do
  it "should have a method called dedependencies" do
    Dependencies.respond_to?(:dependencies).should == true
  end
  it "should have a package(file) method" do
    Dependencies.respond_to?(:package).should == true
  end
end