require File.dirname(__FILE__) + '/../spec_helper'

class TestClass
  include PoolParty::Resources
end
describe "Remote Instance" do
  before(:each) do
    @tc = TestClass.new
  end
  it "should be a string" do
    @tc.has_svnpath({:name => "name", :source => "svn://"}).to_string.should =~ /svnserve \{/
  end
  it "should included the flushed out options" do
    @tc.has_svnpath({:user => "finger"}).to_string.should =~ /user => \"finger\"/
  end
end