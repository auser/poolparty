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
    end
    it "should be a string" do
      @tc.has_git(:name => "gitrepos.git", :source => "git://source.git").to_string.should =~ /exec/
    end
    it "should included the flushed out options" do
      @tc.has_git({:name => "git.git", :source => "git://source.git", :user => "finger"}).to_string.should =~ /finger@git:/
    end
    it "should not include the user if none is given" do
      @tc.has_git({:name => "git.git", :source => "git://source.git"}).to_string.should =~ /git clone git:/
    end
    describe "in resource" do
      before(:each) do
        @tc.instance_eval do
          has_git(:name => "gittr") do
            source "git://source.git"
            path "/var/www/xnot.org"
            symlink "/var/www/xnot.org/public"
          end
        end
      end
      it "should have the path set within the resource" do
        @tc.resource(:git).first.to_string.should =~ /exec \{ "gittr"/
      end
    end
  end
end