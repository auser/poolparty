require File.dirname(__FILE__) + '/../spec_helper'

describe "User" do
  describe "instances" do
    before(:each) do
      @tc = TestBaseClass.new do
        has_user("bob", {:job => "accountant", :comment => "Bob is outstanding"}) do
          password "b0b"
          home "/home/bob"
        end
      end
      @user = @tc.resources[:user].first
    end
    it "have the name in the options" do      
      @user.name.should == "bob"
    end
    it "should store the owner's name" do
      @user.comment.should == "Bob is outstanding"
    end
    it "should store the password (from within the block)" do
      @user.password.should == "b0b"
    end
    it "should store the home" do
      @user.home.should == "/home/bob"
    end
    describe "into PuppetResolver" do
      before(:each) do
        @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
      end
      it "should set the filename to the name of the file" do
        @compiled.should match(/user \{ "bob"/)
      end
      it "set the owner as the owner" do
        # puts "<pre>"+@compiled.to_yaml+"</pre>"
        @compiled.should match(/comment => "Bob is outstanding"/)
      end
      it "should say it's a user in the ensure method" do
        @compiled.should match(/ensure => "present"/)
      end
      it "have the mode set in the puppet output" do
        @compiled.should match(/home => "\/home\/bob"/)
      end
      it "set the password" do
        @compiled.should match(/password => "b0b"/)
      end
    end
  end
end
