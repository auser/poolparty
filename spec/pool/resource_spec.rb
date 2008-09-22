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
      it "should push the template option on to the options" do
        File.stub!(:file?).with("radar").and_return true
        @resource.options.has_key?(:template).should == false
        @resource.template("radar")
        @resource.options.has_key?(:template).should == true
      end
    end
  end
  describe "command" do
    include PoolParty::Resources
    
    it "should create a new 'resource' when calling resource" do
      PoolParty::Resources::File.should_receive(:new).once
      resource(:file)
    end
    it "should create the new 'resource' as a resource" do
      resource(:file).class.to_s.should == "PoolParty::Resources::File"
    end
    it "should receive << when adding a new one" do
      resource(:file).should_receive(:<<).once
      file({:name => "pop"})
    end
    it "should contain 3 instances after calling resource 3 times" do
      file({:name => "red"})
      file({:name => "hot"})
      file({:name => "summer"})
      file.instances.size.should == 3
    end
    describe "adding" do
      before(:each) do
        file({:name => "red"})
        file({:name => "hot"})
        file({:name => "summer"})
        @file2 = file.instance_named("hot")
        @file3 = file.instance_named("summer")
      end
      it "should be able to get a resource by it's name" do
        file.instance_named("hot").class.to_s.should == "PoolParty::Resources::File"
      end
      it "should be able to say it has an instance with a given name" do
        file.contains_instance_named?("hot").should == true
      end
      it "should be able to say that the instance has a name" do
        file.has_name?( @file3 ).should == true
      end
      it "should say it can add an instance if the instance has a name and it is unique" do
        file.can_add_instance?( @file2 ).should == false
      end
    end
    describe "method_missing" do
      before(:each) do
        file({:name => "red"})
        file({:name => "hot"})
        file({:name => "summer"})
        @file2 = file.instance_named("hot")
        @file3 = file.instance_named("summer")
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
        file.instance_named("redface").options[:ensure].should == "present"
      end
      it "should set the does_not_have_file to absent ensure" do
        does_not_have_file({:name => "net"})
        file.instance_named("net").options[:ensure].should == "absent"
      end
      it "should be able to have_service as well" do
        has_service({:name => "apache"})
        service.instance_named("apache").options[:ensure].should == "present"
      end
    end
  end
end
