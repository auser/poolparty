require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/test_plugins/webserver'

class Resource < PoolParty::Resources::Resource
  # Just to give some options for the test class
  def options(h={})
    @options ||= {:a => 1,:b => 2,:c => 3}
  end
end
describe "Resource" do
  before(:each) do
    @resource = Resource.new({:a => 10}) do
      b "90"
    end
  end
  it "should set a from the hash" do
    @resource.a.should == 10
  end
  it "should set b from within the block" do
    @resource.b.should == "90"
  end
  it "should not wipe out the rest of the default options" do
    @resource.c.should == 3
  end
end
