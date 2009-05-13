require File.join(File.dirname(__FILE__), '/../spec_helper')

include PoolParty::Remote
# include Aska

describe "Remote Instance" do
  before(:each) do
    @valid_hash = {:ip => "127.0.0.1", :name => "master", :responding => "true"}
  end

  it "should have tests for the abstaract RemoteInstance class" do
    pending
  end
  
 
end
