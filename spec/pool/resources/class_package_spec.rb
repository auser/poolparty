require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "File" do
  before(:each) do
    @class = PoolParty::Resources::Classpackage.new({:name => "rockstar"})
  end
  it "should have a method resources" do
    @class.respond_to?(:resources).should == true
  end
  it "should store the resources in an array" do
    @class.resources.class.should == Hash
  end
  it "should be able to store resources with <<" do
    @class.respond_to?(:<<).should == true
  end
  describe "with resources" do
    before(:each) do
      @file = file({:name => "red"}) do; end
      @class << @file
    end
    it "should store a resource in the resources array" do
      @class.resources.size.should == 1
    end
    it "should be the file" do
      @class.resource(:file).options.should == @file.options
    end
    describe "to_s" do
      before(:each) do
        @class.instance_eval do
          file({:name => "red"}) do; end
        end
      end
      it "should output the class with the name as class [name]" do
        @class.to_string.should =~ /class rockstar/
      end
      after do
        @class.to_string
      end
    end
  end
end
describe "setting with a block" do
  before(:each) do
    @class1 = class_package do
      name "my_class"
      file({:name => "frank"})
    end
  end
  it "should set the name when set" do
    @class1.name.should == "my_class"
  end
  it "should have the file resource in the resources class" do
    @class1.resources.size.should_not be_zero
  end
  it "should have the file resource in the resources array" do
    @class1.resource(:file).class.should == PoolParty::Resources::File
  end
  it "should store the file in the resources array" do
    @class1.resource(:file).instance_named("frank").name.should == "frank"
  end
end