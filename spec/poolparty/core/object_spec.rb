require File.dirname(__FILE__) + '/../spec_helper'

describe "Object" do
  it "should respond to to_os" do
    Object.new.respond_to?(:to_os).should == true
  end
  describe "methodable object" do
    before(:each) do
      @obj.stub!(:run).and_return "true"
    end
    it "should run the method :run because it exists" do
      @obj.send_if_method(:run).should == "true"
    end
    it "should not run the method :bah because it doesn't exist" do
      @obj.send_if_method("bah").should == "bah"
    end
    it "should not run the method if it is sent nil" do
      @obj.send_if_method(nil).should == nil
    end
    it "should not run an empty string method" do
      @obj.send_if_method("").should == ""
    end
    it "should not run a method that is an integer" do
      @obj.send_if_method(2).should == 2
    end
  end
  describe "with_options" do
    before(:each) do
      class TestObjClass
        include Dslify
        dsl_methods :b
        
        def initialize(o={})
          set_vars_from_options o
        end
      end
      @obj = TestObjClass.new
    end
    it "should respond to with_options" do
      @obj.respond_to?(:with_options).should == true
    end
    it "should set the options on the parent" do
      allow_message_expectations_on_nil
      @a.should_receive(:clone).and_return @a
      @a.stub!(:options).and_return({})
      with_options({:nick => "name"}, @a) do
      end
    end
    
    describe "running" do
      before(:each) do
        @a = TestObjClass.new        
        @b = TestObjClass.new :dude => nil
        
        with_options({:nick => "name", :b => @b}, @a) do
          b.dude = "totally"
        end
        
      end
      it "should set the options on the child in the instance eval" do
        @b.dude.should == "totally"
      end
    end
    describe "contextual running" do
      before(:each) do
        @obj.stub!(:default_options).and_return {}
        @a = @obj.instance_eval { @a = "hello world" }
      end
      it "should have the method meta_eval" do
        @obj.respond_to?(:meta_eval).should == true
      end
    end
  end
end