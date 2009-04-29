require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Resources

require File.dirname(__FILE__) + '/test_plugins/webserver'

class MyResource < PoolParty::Resources::Resource
  # Just to give some options for the test class
  default_options(:a => 1,:b => 2,:c => 3)
end
describe "Resource" do
  before(:each) do
    setup
  end
  describe "wrapped" do
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
    describe "class methods" do
      it "should have an array of available resources" do
        PoolParty::Resources::Resource.available_resources.class.should == Array
      end
      it "should not be empty" do
        PoolParty::Resources::Resource.available_resources.empty?.should == false
      end
    end
    describe "instance methods" do
      before(:each) do
        @resource = MyResource.new
      end
      %w(requires ensures is_present is_absent ifnot).each do |method|
        eval <<-EOE
          it "should have the method #{method} available" do; @resource.respond_to?(:#{method}).should == true; end
        EOE
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
        @resource.options.has_key?(:ensures).should == false
        @resource.ensures :absent
        @resource.options.has_key?(:ensures).should == true
      end
      it "should write the option ensures as present with is_present" do
        @resource.options.has_key?(:ensures).should == false
        @resource.is_present
        @resource.options.has_key?(:ensures).should == true
      end
      it "should write the option ensures as absent with is_absent" do
        @resource.options.has_key?(:ensures).should == false
        @resource.is_absent
        @resource.options.has_key?(:ensures).should == true
      end
      it "should write the option unless for ifnot" do
        @resource.options.has_key?(:unless).should == false
        @resource.ifnot "str"
        @resource.options[:unless].should == "str"
      end
    end
    describe "command" do
      before(:each) do
        @tc = TestBaseClass.new do
          has_file(:name => "frank")
        end
        @cloud = @tc
      end
      describe "add_resource" do
        it "should call add_resource when creating using the command: file" do
          @cloud.should_receive(:add_resource).with(:file, {:name => "frank"})
          @cloud.instance_eval do
            __file(:name => "frank")
          end
        end
        it "should return a resource when the resource does not exist" do
          @cloud.resource(:file).first.class.should == PoolParty::Resources::File
        end
        it "should return a resource when the resource does exist" do
          @cloud.instance_eval do
            resources[:file] = nil
            has_file({:name => "frank"})
          end
          @cloud.resource(:file).first.class.should == PoolParty::Resources::File
        end
      end
      it "should create the new 'resource' as a resource" do
        @tc.resource(:file).class.should == Array
      end
      it "should contain 3 instances after calling resource 3 times" do
        @cloud.instance_eval do
          resources[:file] = nil
          has_file({:name => "red"})
          has_file({:name => "hot"})
          has_file({:name => "summer"})
        end
        @tc.resource(:file).size.should == 3
      end
      describe "adding" do
        before(:each) do
          @cloud.instance_eval do
            resources[:file] = nil
            has_file({:name => "red"})
            has_file({:name => "hot"})
            has_file({:name => "summer"})
          end
        end
        it "should contain file named with 'red'" do
          @cloud.get_resource(:file, "red").nil?.should == false
        end
      end
      describe "method_missing" do
        before(:each) do
          @cloud.instance_eval do
            resources[:file] = nil
            has_file({:name => "red"})
            has_file({:name => "hot"})
            has_file({:name => "summer"})
          end
        end
        it "should be able to pick out methods with the phrase has_" do
          lambda {
            @cloud.instance_eval do
              has_file({:name => "summer"})
            end
          }.should_not raise_error
        end
        it "should pick out methods with the phrase does_not_" do
          lambda {
            @cloud.instance_eval do
              does_not_have_file({:name => "summer"})
            end
          }.should_not raise_error
        end
      end
      describe "get_resource" do
        before(:each) do
          @tc.instance_eval do
            resources[:file] = nil
            has_file(:name => "red")
            has_file(:name => "hot")
            has_file(:name => "tamales")
          end
        end
        it "should return a type of resource when looking for a resource that exists" do
          @tc.get_resource(:file, "hot").class.should == PoolParty::Resources::File
        end
        it "should return the resource of the name requested" do
          @tc.get_resource(:file, "hot").name.should == "hot"
        end
        it "should return nil if the resource requested is not there" do
          @tc.get_resource(:file, "smarties").nil?.should == true
        end
        it "should not have created any more resources" do
          @tc.resource(:file).size.should == 3
        end
      end
      describe "appending to resource" do
        before(:each) do
          @tc = TestBaseClass.new do
            has_directory(:name => "/var/www") do
              has_file(:name => "/var/www/file.html")
            end
          end
          @dir = @tc.resource(:directory).first
          @file = @tc.resource(:file).first
        end
        it "should say there is 1 resource because the lower-level resources should be contained on the parenting resource" do
          @tc.resources.size.should == 2
        end
        it "should say there is one resource on the outer resource" do
          @dir.resources.size.should == 2
        end
        it "should contain the file as a resource" do
          @dir.resource(:file)[0].class.should == PoolParty::Resources::File
        end
      end
      describe "fetching" do
        before(:each) do
          @tc = TestBaseClass.new do
            has_file(:name => "pancakes")
          end
          @file = @tc.resource(:file).first
        end
        it "should return the file preiously created" do
          @tc.resource(:file).first.options.keys.sort.should == @file.options.keys.sort
        end
      end
    end    
  end
end
