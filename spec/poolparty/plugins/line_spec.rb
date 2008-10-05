require File.dirname(__FILE__) + '/../spec_helper'

class TestClass
  include PoolParty::Resources
end
describe "Remote Instance" do
  before(:each) do
    @tc = TestClass.new
  end
  it "should be a string" do
    @tc.has_line_in_file("hi", "ho").to_string.should =~ /line \{/
  end
  it "should included the flushed out options" do
    @tc.has_line_in_file("hi", "who", {:name => "finger"}).to_string.should =~ /name => 'finger'/
  end
end