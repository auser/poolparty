require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Gem" do
  before(:each) do
    reset!
    gem(:name => "rails")
  end
  it "should have a package" do
    package.should_not be_empty
  end
  it "should have the 'rails' gem in the packages" do
    package.instance_named("rails").to_string.should =~ /Package\[rubygems\]/
  end
end