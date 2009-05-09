require File.dirname(__FILE__) + '/../spec_helper'
require "open-uri"

describe "basic" do
  before(:each) do
    PoolParty.reset!
    @example_spec_file = ::File.join(::File.dirname(__FILE__), "..", "..", "..", "examples", 'basic.rb')
    set_pool_specfile @example_spec_file
    PoolParty::Pool::Pool.load_from_file(@example_spec_file)
    @db = clouds[:basic_db]
    @app = clouds[:basic_app]
  end
  it "should have one pool called :app" do
    pool(:application).should_not == nil
    pools[:application].should_not == nil
  end
  it "should have a cloud called :app" do
    clouds[:basic_app].should_not == nil
  end
  it "should have a cloud called :db" do
    pools[:application].clouds[:basic_db].should_not == nil
  end
  it "should set the minimum_instances on the cloud to 2 (overriding the pool options)" do    
    pools[:application].minimum_instances.should == 3
    clouds[:basic_app].minimum_instances.should == 12
  end
  it "should set the maximum_instances on the cloud to 50" do
    clouds[:basic_app].maximum_instances.should == 50
  end
  it "should set the minimum_instances on the db cloud to 3" do
    clouds[:basic_db].minimum_instances.should == 19
    clouds[:basic_app].minimum_instances.should == 12
    pools[:application].minimum_instances.should ==3
  end
  it "should set the parent to the pool" do
    clouds[:basic_app].parent.should == pools[:application]
    clouds[:basic_db].parent.should == pools[:application]
    clouds[:basic_db].parent.should_not == pools[:app]
  end
  it "should have the keypair matching /auser/on the db cloud " do
    clouds[:basic_db]._keypairs.select{|a| a.filepath.match(/auser/)}
  end
  it "cloud should know what remoter base it is using" do
    clouds[:basic_db].remote_base.class.should == PoolParty::Remote::Vmrun
  end
  it "cloud should have methods from the remoter base available" do
    clouds[:basic_db].remote_base.should_receive(:describe_instances).and_return({})
    clouds[:basic_db].describe_instances.should == {}
  end
  it "should not return nil to undefined methods" do
    lambda {clouds[:basic_db].not_a_method_that_exists_anywhere}.should raise_error
  end
  it "should have the keypair set for the specific cloud on top of the keypair stack" do
    #I think this should be the behavior. mf
    # pools[:application].clouds[:basic_db].keypairs.last.filepath.should_match(/auser/)
  end
end