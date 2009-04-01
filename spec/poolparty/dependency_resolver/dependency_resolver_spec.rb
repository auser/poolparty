require File.dirname(__FILE__) + '/../spec_helper'

describe "Resolution spec" do
  before(:each) do
    @dr = DependencyResolver.new({})
    DependencyResolver.stub!(:new).and_return @dr
  end
  
  it "should have a tree_hash associated with the resolver" do
    @dr.properties_hash.should == {}
  end
  it "runs compile on a new object when calling compile on the class" do    
    @dr.should_receive(:compile)
    DependencyResolver.compile(hash)
  end
end