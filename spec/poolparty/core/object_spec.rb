require File.dirname(__FILE__) + '/../spec_helper'

describe "Object" do
  it "should respond to to_os" do
    Object.new.respond_to?(:to_os).should == true
  end
  describe "with_options" do
    before(:each) do
      @obj = Class.new
    end
    it "should respond to with_options" do
      @obj.respond_to?(:with_options).should == true
    end
    it "should set the options on the parent" do
      @a.should_receive(:clone).and_return @a
      @a.stub!(:options).and_return({})
      with_options({:nick => "name"}, @a) do
      end
    end
    
    describe "running" do
      before(:each) do
        Class.stub!(:default_options).and_return({})
        Class.send :include, Configurable
        Class.send :include, MethodMissingSugar
        @a = Class.new        
        @b = Class.new
        
        with_options({:nick => "name", :b => @b}, @a) do
          b.dude "totally"
        end
        
      end
      it "should set the options on the child in the instance eval" do
        @b.dude.should == "totally"
      end
    end
  end
end