require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Monitors

describe "CpuMonitor" do
  before(:each) do
    @mon = CpuMonitor.new
    @mon.stub!(:run).and_return 0.5
  end
  it "should have the singleton method run defined" do
    CpuMonitor.respond_to?(:run).should == true
  end
  it "should call the method new" do
    CpuMonitor.should_receive(:new).and_return @mon
    CpuMonitor.run
  end
end