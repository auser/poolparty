require File.dirname(__FILE__) + '/../spec_helper'

describe "Array" do
  before(:each) do
    @array = ["run this", "run that"]
  end
  describe "runnable" do
    it "should be able to produced a runnable string" do;@array.runnable.should == 'run this  &&  run that';end
    it "should be turn into a string" do;@array.runnable.class.should == String;end
  end
  describe "nice_runnable" do
    before(:each) do
      @array << ""
      @array << "peabody"      
    end
    it "should reject any empty elements out of the array" do
      (@array << [""]).nice_runnable.should == "run this \n run that \n peabody"
    end
  end
  describe "to_os" do
    it "should be able to turn itself to an open struct" do; @array.first.should_receive(:to_os);end        
    after do
      @array.to_os
    end
  end
end