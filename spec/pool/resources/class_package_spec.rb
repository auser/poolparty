require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "File" do
  before(:each) do
    @class = PoolParty::Resources::ClassPackage.new
  end
  it "should have a method resources" do
    @class.respond_to?(:resources).should == true
  end
  it "should store the resources in an array" do
    @class.resources.class.should == Array
  end
  it "should be able to store resources with <<" do
    @class.respond_to?(:<<).should == true
  end
  describe "with resources" do
    before(:each) do
      @file = file do; end
      @class << @file
    end
    it "should store a resource in the resources array" do
      @class.resources.size.should == 1
    end
    it "should be the file" do
      @class.resources.first.should == @file
    end
    describe "to_s" do
      it "should call to_s on the file" do
        @file.should_receive(:to_s).and_return "file {}"
      end
      it "should output the class with the name as class [name]" do
        @class.to_s.should =~ /class custom/
      end
      after do
        @class.to_s
      end
    end
  end
end
describe "setting with a block" do
  before(:each) do
    @class1 = class_package do
      name "my_class"
      file({:name => "frank"}) do; end
    end
  end
  it "should set the name when set" do
    @class1.name.should == "my_class"
  end
  it "should have the file resource in the resources class" do
    @class1.resources.size.should_not be_zero
  end
  it "should have the file resource in the resources array" do
    @class1.resources.first.class.should == PoolParty::Resources::File
  end
  it "should store the file in the resources array" do
    @class1.resources.first.name.should == "frank"
  end
end