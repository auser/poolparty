require File.dirname(__FILE__) + '/../spec_helper'

include Remote

describe "Remote Instance" do
  before(:each) do
    @valid_hash = {:ip => "127.0.0.1", :name => "master", :responding => "true", :load => "0.5"}
    @valid_string = "127.0.0.1 master"
  end
  it "should create a remote instance with a Hash" do
    @ri = RemoteInstance.new(@valid_hash)
    @ri.valid?.should == true
  end
  it "should create a valid remote instance with a String" do
    @ri = RemoteInstance.new(@valid_string)
    @ri.valid?.should == true
  end
  it "should not be valid if there is no ip associated" do
    @ri = RemoteInstance.new(@valid_hash.merge({:ip => nil}))
    @ri.valid?.should == false
  end
  it "should not be valid if there is no name associated" do
    @ri = RemoteInstance.new(@valid_hash.merge({:name => nil}))
    @ri.valid?.should == false
  end
end