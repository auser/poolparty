require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::DefinableResource

context "define_resource" do
  it "should have the method define_resource available" do
    self.respond_to?(:define_resource).should == true
  end
end