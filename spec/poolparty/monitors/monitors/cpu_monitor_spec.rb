require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Monitors

describe "CpuMonitor" do
  before(:each) do
    @mon = CpuMonitor.new
  end
  it "should have the singleton method run defined" do
    CpuMonitor.respond_to?(:run).should == true
  end
  it "should call the method new" do
    CpuMonitor.should_receive(:new).and_return @mon
    CpuMonitor.run
  end
end