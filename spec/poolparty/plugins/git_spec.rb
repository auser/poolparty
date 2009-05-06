require File.dirname(__FILE__) + '/../spec_helper'

class TestGitClass < PoolParty::Cloud::Cloud
end

describe "Remote Instance" do
  describe "wrapped" do
    before(:each) do
      reset!
      @tc = cloud :test_git_class_cloud do
        has_file "/var/www/bino"
        has_git_repo "git://git/repos/source.git", :to => "/var/www/source", :requires_user => "finger"
      end
      @compiled = ChefResolver.new(@tc.to_properties_hash).compile
    end
    it "should be a string" do
      @compiled.should =~ /execute/
    end
    it "should included the flushed out options" do
      @compiled.should =~ /finger@git:/
    end
    it "should not include the user if none is given" do
      @compiled.should =~ /git clone finger@git:/
    end
    describe "in resource" do
      before(:each) do
        @tc = cloud :test_git_class_cloud_two do
          has_git_repo("git://source.git") do
            to "/var/www/xnot.org"
          end
        end
      end
      it "should have the path set within the resource" do
        ChefResolver.new(@tc.to_properties_hash).compile.should =~ /execute \"git-git:\/\/source\.git/
      end
    end
  end
end