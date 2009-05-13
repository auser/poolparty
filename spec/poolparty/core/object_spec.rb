require File.dirname(__FILE__) + '/../spec_helper'

describe "Object" do
  it "should respond to to_os" do
    Object.new.respond_to?(:to_os).should == true
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
