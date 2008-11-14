require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Resources

class TestClass
  include PoolParty::Resources
end
describe "Remote Instance" do
  before(:each) do
    reset_resources!
  end
  describe "wrapped" do
    before(:each) do
      @tc = TestClass.new      
      @cloud = MyOpenStruct.new(:keypair => "keys", :remote_keypair_path => "/keypair_path", :name => "cloudcloud")
      @cloud.stub!(:is_a?).with(PoolParty::Cloud::Cloud).and_return true
      @tc.stub!(:parent).and_return @cloud
      
      @options = {:name => "deploydirectory", :from => ::File.dirname(__FILE__), :to => "/var/www/deploydirectory", :testing => false}
    end
    it "should be a string" do
      @tc.has_deploydirectory(@options).to_string.should =~ /exec \{/
    end
    it "should included the flushed out options" do
      @tc.has_deploydirectory(@options).to_string.should =~ /command/
    end
    describe "in resource" do
      before(:each) do
        @tc.instance_eval do
          has_deploydirectory do
            name "deploydirectory"
            from ::File.dirname(__FILE__)
            to "/var/www/deploydirectory"
          end
        end
      end
      it "should have the path set within the resource" do
        @tc.resource(:deploydirectory).first.to_string.should =~ /exec \{/
      end
      it "should not have the from in the to_string" do
        @tc.resource(:deploydirectory).first.to_string.should_not =~ /from /
      end
      it "should not have the to in the to_string" do
        @tc.resource(:deploydirectory).first.to_string.should_not =~ /to /
      end
    end
  end
end