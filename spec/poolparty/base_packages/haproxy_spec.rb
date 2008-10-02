require File.dirname(__FILE__) + '/../spec_helper'

describe "haproxy base package" do
  before(:each) do
    @ha = PoolPartyHaproxyClass.new
    stub_list_from_remote_for(@ha)
  end
  it "should have the heartbeat package defined" do
    lambda {PoolPartyHaproxyClass}.should_not raise_error    
  end
  it "should call enable (and setup resources) since there is no block given when it's instantiated" do
    @ha.resources.should_not be_empty
  end
  it "should have no resources when starting with a block (that defines no methods)" do
    @pphc = PoolPartyHaproxyClass.new do
    end
    @pphc.resources.should be_empty
  end
  it "should have a file resource" do
    @ha.resource(:package).should_not be_empty
  end
end