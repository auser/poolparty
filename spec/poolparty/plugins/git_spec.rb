require File.dirname(__FILE__) + '/../spec_helper'

class TestClass
  include PoolParty::Resources
end
describe "Remote Instance" do
  before(:each) do
    @tc = TestClass.new
  end
  it "should be a string" do
    @tc.has_git({:name => "gitrepos.git", :source => "git://source.git"}).to_string.should =~ /file \{/
  end
  it "should included the flushed out options" do
    @tc.has_git({:name => "git.git", :source => "git://source.git", :user => "finger"}).to_string.should =~ /finger@git:/
  end
  it "should not include the user if none is given" do
    @tc.has_git({:name => "git.git", :source => "git://source.git"}).to_string.should =~ /git clone git:/
  end
end