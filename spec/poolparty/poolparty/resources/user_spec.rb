require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "User" do
  describe "instances" do
    before(:each) do
      @user = remote_user({:name => "fred"})
    end
    it "should turn the one hash instance into a string" do
      @user.to_string.should =~ /"fred":/
    end
    it "should turn the two hash instance into a string" do
      @user = remote_user do
        name "bob"
        home "/home/bob"
      end
      @user.to_string.should =~ /"bob":/
      @user.to_string.should =~ /home => '\/home\/bob'/
    end
    describe "as included" do            
      before(:each) do
        @user = remote_user({:rent => "low"}) do
          name "/www/conf/httpd.conf"
        end
      end
      it "should use default values" do
        @user.name.should == "/www/conf/httpd.conf"
      end
      it "should keep the default values for the user" do
        @user.shell.should == "/bin/sh"
      end
      it "should also set options through a hash" do
        @user.rent.should == "low"
      end
    end
  end
end
