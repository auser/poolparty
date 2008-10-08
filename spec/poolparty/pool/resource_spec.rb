require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/test_plugins/webserver'

class MyResource < PoolParty::Resources::Resource
  # Just to give some options for the test class
  def options(h={})
    @options ||= {:a => 1,:b => 2,:c => 3}
  end
end
describe "Resource" do
  before(:each) do
    @resource = MyResource.new({:a => 10}) do
      b "90"
    end
  end
  it "should set a from the hash" do
    @resource.a.should == 10
  end
  it "should set b from within the block" do
    @resource.b.should == "90"
  end
  it "should not wipe out the rest of the default options" do
    @resource.c.should == 3    
  end
  describe "to_s" do
    it "should be able to coalesce the instances" do
      @resource.to_string.should =~ /resource \{\n/
    end
    describe "with resources" do
      before(:each) do
        self.stub!(:options).and_return(:name => "cook")
        
        @obj = PoolParty::Resources::Resource.new
        @obj.stub!(:name).and_return "cook"
        
        @resource2 = MyResource.new do
          file(:name => "shulie")
        end
      end
      it "should call classpackage_with_self when it has resources" do
        @resource2.should_receive(:classpackage_with_self).and_return @obj
        @resource2.to_string
      end
      it "should call to_string on the class package" do
        @obj.should_receive(:to_string).and_return "wee"
        @resource2.should_receive(:classpackage_with_self).and_return @obj
        @resource2.to_string
      end
    end
  end
  describe "class methods" do
    it "should have an array of available resources" do
      PoolParty::Resources::Resource.available_resources.class.should == Array
    end
    it "should not be empty" do
      PoolParty::Resources::Resource.available_resources.should_not be_empty
    end
  end
  describe "instance methods" do
    before(:each) do
      @resource = MyResource.new
    end
    it "should be able to take requires method" do
      @resource.respond_to?(:requires).should == true
    end
    it "should push require onto the options" do
      @resource.options.has_key?(:require).should == false
      @resource.requires("nibbles")
      @resource.options.has_key?(:require).should == true
    end
    it "should be able to call ensures method on the resource" do
      @resource.respond_to?(:ensures).should == true
    end
    it "should push the option ensure onto the options" do
      @resource.options.has_key?(:ensure).should == false
      @resource.ensures("nibbles")
      @resource.options.has_key?(:ensure).should == true
    end
    describe "templating" do
      before(:each) do
        FileUtils.stub!(:cp).and_return true        
      end
      it "should have the method template" do
        @resource.respond_to?(:template).should == true
      end
      it "should raise an exception if no file is given" do
        lambda {
          @resource.template
        }.should raise_error
      end
      it "should raise an excepton if the file cannot be found" do
        lambda {
          @resource.template("radar")
        }.should raise_error
      end
      it "should not raise an exception if there is a file passed and the file is found" do
        File.should_receive(:file?).with("radar").and_return true
        lambda {
          @resource.template("radar")
        }.should_not raise_error
      end
      # it "should push the template option on to the options" do
      #   File.stub!(:file?).with("radar").and_return true
      #   @resource.options.has_key?(:template).should == false
      #   @resource.template("radar")
      #   @resource.options.has_key?(:template).should == true
      # end
    end
  end
  describe "command" do
    include PoolParty::Resources
    before(:each) do
      reset_resources!
    end
    it "should create the new 'resource' as a resource" do
      resource(:file).class.should == Array
    end
    it "should receive << when adding a new one" do
      resource(:file).should_receive(:<<).once
      file({:name => "pop"})
    end
    it "should contain 3 instances after calling resource 3 times" do
      file({:name => "red"})
      file({:name => "hot"})
      file({:name => "summer"})
      resource(:file).size.should == 3
    end
    describe "adding" do
      before(:each) do
        @a = file({:name => "red"})
        @b = file({:name => "hot"})
        @c = file({:name => "summer"})
      end
      it "should contain file named with 'red'" do
        resource(:file).include?(@a).should == true
      end
    end
    describe "method_missing" do
      before(:each) do
        file({:name => "red"})
        file({:name => "hot"})
        file({:name => "summer"})
      end
      it "should be able to pick out methods with the phrase has_" do
        lambda {
          has_file
        }.should_not raise_error
      end
      it "should not have a method prepended with crabs_" do
        lambda {
          crabs_file
        }.should raise_error
      end
      it "should pick out methods with the phrase does_not_" do
        lambda {
          does_not_have_file({:name => "red"})
        }.should_not raise_error
      end
      it "should set the has_file to present ensure" do
        has_file({:name => "redface"})
        resource(:file).get_named("redface").first.options[:ensure].should == "present"
      end
      it "should set the does_not_have_file to absent ensure" do
        does_not_have_file({:name => "net"})
        resource(:file).get_named("net").first.options[:ensure].should == "absent"
      end
      it "should be able to have_service as well" do
        has_service({:name => "apache"})
        resource(:service).get_named("apache").first.options[:ensure].should == "running"
      end
    end
    describe "get_resource" do
      before(:each) do
        @red = file(:name => "red")
        @hot = file(:name => "hot")
        @tamales = file(:name => "tamales")
      end
      it "should return a type of resource when looking for a resource that exists" do
        get_resource(:file, "hot").class.should == PoolParty::Resources::File
      end
      it "should return the resource of the name requested" do
        get_resource(:file, "hot").should == @hot
      end
      it "should return nil if the resource requested is not there" do
        get_resource(:file, "smarties").should be_nil
      end
    end
  end
end
