require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/../../lib/poolparty/helpers/display'

include Display

describe "Display" do
  it "should respond to pool_describe" do
    self.respond_to?(:pool_describe).should == true
  end
  it "should be able to get the list of remote bases" do
    self.respond_to?(:available_bases).should == true
  end
  it "should call Remote.available_bases when calling available_bases" do
    Remote.should_receive(:available_bases).once
    available_bases
  end
  it "should look through the directory of remote_bases and respond with those" do
    Dir.stub!(:[]).and_return ["ec2"]
    available_bases.should == ["ec2"]
  end
end