require File.dirname(__FILE__) + '/../spec_helper'

describe "haproxy base package" do
  before(:each) do
    @ha = PoolPartyHaproxyClass.new do      
    end
    stub_list_from_remote_for(@ha)
    @ha.stub!(:list_of_running_instances).and_return []
  end
  it "should have the heartbeat package defined" do
    lambda {PoolPartyHaproxyClass}.should_not raise_error    
  end
end