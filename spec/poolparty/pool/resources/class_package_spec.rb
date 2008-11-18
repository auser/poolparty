require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "File" do
  before(:each) do
    reset_resources!  
  end
  describe "wrapped" do
      before(:each) do
        reset!
        @class = PoolParty::Resources::Classpackage.new({:name => "rockstar"})
      end
      it "should have a method resources" do
        @class.respond_to?(:resources).should == true
      end
      it "should store the resources in an array" do
        @class.resources.class.should == Hash
      end
      describe "with resources" do
        before(:each) do
          cloud :application_cloud do
            classpackage do
              file({:name => "red"})
            end
          end
          @class = cloud(:application_cloud).resource(:classpackage).first
        end
        it "should store a resource in the resources array" do
          @class.resources.size.should == 1
        end
        it "should have a file resource in the cloud" do
          @class.resource(:file).should_not == nil
        end
        describe "to_s" do
          before(:each) do
            @class.instance_eval do
              name "rockstar"
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
        @class1 = classpackage do
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
        @class1.resource(:file).class.should == Array
      end
      it "should store the file in the resources array" do
        @class1.resource(:file).get_named("frank").first.name.should == "frank"
      end
    end
    describe "from a collection of resources to another" do
      before(:each) do 
        reset_resources!       
        cloud :bunkers do
          file(:name => "franksfile")
          exec(:name => "get file", :command => "kill frank for file")
        end
        @cloud = cloud(:bunkers)
        @class2 = classpackage_with_self(@cloud)
      end
      it "should have the method classpackage_with_self" do
        self.respond_to?(:classpackage_with_self).should == true
      end
      it "should transfer the resources to the class" do
        @class2.resources.size.should == 2
      end
      it "should have the file resource in the classpackage" do
        @class2.resource(:file).first.name.should == "franksfile"
      end
      it "should have the exec resource in the classpackage" do
        @class2.resource(:exec).first.name.should == "get file"
      end
      it "should have the resources on the new classpackage" do
        @cloud.resources.size.should == 1
      end
      it "should have the conditional classpackage on the resources" do
        @cloud.resource(:classpackage).first.should == @class2
      end
      describe "to_string" do
        before(:each) do
          @output = @class2.to_string
        end
        it "should have the file in the string" do
          @output.should =~ /franksfile/
        end
        it "should contain just the two resources in the string" do
          @output.match(/(\w+) \{/).size.should == 2
        end
        describe "from within the cloud" do
          before(:each) do
            reset_resources!
            @output = @cloud.build_short_manifest
            puts @output
          end
          it "should have one class" do
            @output.match(/class (\w+) \{/).size.should == 1
          end
        end
      end
  end
end