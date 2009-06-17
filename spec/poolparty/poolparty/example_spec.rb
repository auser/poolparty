require File.dirname(__FILE__) + '/../spec_helper'
require "open-uri"

# pool :application do
#   instances 1..5
#   
#   cloud :frontend do
#     minimum_instances 3
#     keypair 'front'
#     image_id "ami-abc123"
#     has_file :name => "/etc/motd", :content => "Welcome to your PoolParty instance"
#   end
#   
#   cloud :database do
#     using :vmrun do
#       vmx_hash "/path/to/vmx_file" => "192.168.248.122"
#     end
#     minimum_instances 1
#     image_id "ami-1234bc"
#   end
# 
# end

describe "basic" do
  before(:each) do
    PoolParty.reset!
    @example_spec_file = ::File.join(::File.dirname(__FILE__), "..", "..", "..", "examples", 'basic.rb')
    set_pool_specfile @example_spec_file
    PoolParty::Pool::Pool.load_from_file(@example_spec_file)
    @db = clouds[:database]
    @app = clouds[:frontend]
  end
  it "should have a remote base" do
    clouds[:frontend].remoter_base.should == :ec2
    clouds[:frontend].remote_base.class.should == Ec2
  end
  it "should have one pool called :app" do
    pool(:application).should_not == nil
    pools[:application].should_not == nil
  end
  it "should have a cloud called :app" do
    clouds[:frontend].should_not == nil
  end
  it "should have a cloud called :db" do
    pools[:application].clouds[:database].should_not == nil
  end
  it "should set the minimum_instances on the cloud (overriding the pool options)" do    
    pools[:application].minimum_instances.should == 1
    clouds[:frontend].minimum_instances.should == 3
  end
  it "should set the maximum_instances on the cloud to 5" do
    clouds[:frontend].maximum_instances.should == 5
  end
  it "should set the minimum_instances on the db cloud " do
    clouds[:database].minimum_instances.should == 1
    clouds[:frontend].minimum_instances.should == 3
    pools[:application].minimum_instances.should == 1
  end
  it "should set the parent to the pool" do
    clouds[:frontend].parent.should == pools[:application]
    clouds[:database].parent.should == pools[:application]
    clouds[:database].parent.should_not == pools[:app]
  end
  it "should have the keypair matching /auser/on the db cloud " do
    clouds[:database]._keypairs.select{|a| a.filepath.match(/auser/)}
  end
  it "cloud should know what remoter base it is using" do
    clouds[:database].remote_base.class.should == PoolParty::Remote::Vmrun
  end
  it "cloud should have methods from the remoter base available" do
    clouds[:database].remote_base.should_receive(:describe_instances).and_return({})
    clouds[:database].describe_instances.should == {}
  end
  it "should not return nil to undefined methods" do
    lambda {clouds[:database].not_a_method_that_exists_anywhere}.should raise_error
  end
  it "should have the keypair set for the specific cloud on top of the keypair stack" do
    #I think this should be the behavior. mf
    # pools[:application].clouds[:database].keypairs.last.filepath.should_match(/auser/)
  end
end