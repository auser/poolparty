require File.dirname(__FILE__) + '/../spec_helper'

describe "Array" do
  before(:each) do
    @array = ["run this", "run that"]
  end
  describe "runnable" do
    it "should be able to produced a runnable string" do
                                                        @array.runnable.should == 'run this  &&  run that';end
    it "should be turn into a string" do
                                                        @array.runnable.class.should == String;end
  end  
  describe "to_os" do
    it "should be able to turn itself to an open struct" do; @array.first.should_receive(:to_os);end    
    
    after do
      @array.to_os
    end
  end
end