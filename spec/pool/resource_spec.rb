require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/test_plugins/webserver'

class Resource < PoolParty::Resources::Resource
  # Just to give some options for the test class
  def options(h={})
    @options ||= {:a => 1,:b => 2,:c => 3}
  end
end
describe "Resource" do
  before(:each) do
    @resource = Resource.new({:a => 10}) do
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
  end
end
