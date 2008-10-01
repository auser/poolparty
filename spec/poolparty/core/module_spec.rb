require File.dirname(__FILE__) + '/../spec_helper'

describe "Module" do
  before(:each) do
    @mod = Module.new do
      attr_accessor_with_default :a do
        Hash.new
      end
    end
    @klass = Class.new.extend(@mod)
  end
  it "should be able to set method accessors" do
    @klass.a.should == {}
  end
end