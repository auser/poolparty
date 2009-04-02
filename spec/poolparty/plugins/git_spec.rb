require File.dirname(__FILE__) + '/../spec_helper'

class TestGitClass < PoolParty::Cloud::Cloud
end

describe "Remote Instance" do
  describe "wrapped" do
    before(:each) do
      reset!
      @tc = cloud :test_git_class_cloud do
        has_git_repos :at => "/var/www/", :name => "gitrepos.git", :source => "git://git/repos/source.git", :requires_user => "finger"
      end
      @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
    end
    it "should be a string" do
      @compiled.should =~ /exec/
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
          has_git_repos(:name => "gittr") do
            symlink "/var/www/xnot.org/public"
            source "git://source.git"
            path "/var/www/xnot.org"       
            at "/var/www"
          end
        end
      end
      it "should have the path set within the resource" do
        PuppetResolver.new(@tc.to_properties_hash).compile.should =~ /exec \{ \"git-gittr/
      end
    end
  end
end