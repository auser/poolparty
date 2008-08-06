require File.dirname(__FILE__) + '/../spec_helper'

describe "Float" do
  it "should be able to round to the nearest integer" do
    10.95.round_to(1).should == 11
  end
  it "should be able to get the ceiling" do
    10.54.ceil_to(1).should == 10.6
  end
  it "should be able to get the floor" do
    10.14.floor_to(1).should == 10.1
  end
end