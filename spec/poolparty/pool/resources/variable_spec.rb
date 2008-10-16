require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Variable" do
  before(:each) do
    reset!
    @variable = variable(:name => "name", :value => "ari Lerner")
  end
  it "should have a package" do
    resource(:variable).should_not be_empty
  end
  it "should have the 'name' variable set as a variable" do
    @variable.to_string.should =~ /\$name = 'ari Lerner'/
  end
  it "should be able to set the variable as an array if passed an array" do
    v = variable(:name => "girlfriends", :value => %w(Rachel Erica Michelle))
    v.to_string.should == "$girlfriends = [ 'Rachel', 'Erica', 'Michelle' ]"
  end
  it "should set the string to be without a string if it's a function" do
    v = variable(:name => "girlfriends", :value => "lookup_var()")
    v.to_string.should == "$girlfriends = lookup_var()"
  end
end