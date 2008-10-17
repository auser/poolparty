require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Monitors

describe "Monitors" do
  it "should have a list of available monitors" do
    PoolParty::Monitors.available_monitors.empty?.should == false
  end
  it "should register a module and append it to the available monitors" do
    size = PoolParty::Monitors.available_monitors.size
    PoolParty::Monitors.register_monitor :fake
    size.should == PoolParty::Monitors.available_monitors.size - 1
  end
end
describe "BaseMonitor" do
  it "should have the singleton method run defined" do
    BaseMonitor.respond_to?(:run).should == true
  end
end