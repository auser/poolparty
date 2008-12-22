require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Symlink" do
  before(:each) do
    reset_resources!
    @cloud = cloud :symlink_test do
      has_symlink(:name => "/etc/apache2/puppetmaster.conf")
    end
    @symlink = @cloud.resource(:symlink).first
  end
  it "should create a file" do
    @cloud.resource(:symlink).empty?.should == false
  end
  it "should create a file { resource" do
    @symlink.to_string.should =~ /file \{/
  end
  it "should use the from for the ensures" do
    @symlink.ensure.should == @symlink.source
  end
end