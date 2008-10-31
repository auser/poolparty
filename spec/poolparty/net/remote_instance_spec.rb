require File.dirname(__FILE__) + '/../spec_helper'

include Remote

describe "Remote Instance" do
  before(:each) do
    setup
    @valid_hash = {:ip => "127.0.0.1", :name => "master", :responding => "true"}
  end
  describe "configurable" do
    it "should set the options sent in the options" do
      @ri = RemoteInstance.new(@valid_hash, nil)
      @ri.ip.should == "127.0.0.1"
    end
    it "should set the options sent by the parent" do
      @obj = Object.new
      @obj.stub!(:options).and_return({:dude => "tte"})
      @ri = RemoteInstance.new(@valid_hash, @obj)
      @ri.dude.should == "tte"
    end
    it "should not overwrite the options that are already set" do
      @obj = Object.new
      @obj.stub!(:options).and_return({:ip => "172.176.0.1"})
      @ri = RemoteInstance.new(@valid_hash, @obj)
      @ri.ip.should == "127.0.0.1"
    end
  end
  it "should create a remote instance with a Hash" do
    @ri = RemoteInstance.new(@valid_hash, nil)
    @ri.valid?.should == true
  end
  it "should not be valid if there is no ip associated" do
    @ri = RemoteInstance.new(@valid_hash.merge({:ip => nil}), nil)
    @ri.valid?.should == false
  end
  it "should not be valid if there is no name associated" do
    @ri = RemoteInstance.new(@valid_hash.merge({:name => nil}), nil)
    @ri.valid?.should == false
  end
  describe "status" do
    it "should say it is running when the status == running" do
      RemoteInstance.new(@valid_hash.merge(:status => "running")).running?.should == true
    end
    it "should say it is pending when the status == pending" do
      RemoteInstance.new(@valid_hash.merge(:status => "pending")).pending?.should == true
    end
    it "should say it is terminating when the status == shutting down" do
      RemoteInstance.new(@valid_hash.merge(:status => "shutting down")).terminating?.should == true
    end
    it "should say it is terminated when the status == terminated" do
      RemoteInstance.new(@valid_hash.merge(:status => "terminated")).terminated?.should == true
    end
    it "should not say it is running when it is pending" do
      RemoteInstance.new(@valid_hash.merge(:status => "pending"), nil).running?.should == false
    end
  end
  describe "methods" do
    before(:each) do
      @ri = RemoteInstance.new(@valid_hash, nil)
    end
    it "should be say that it is the master if the name is master" do
      @ri.name.should == "master"
      @ri.master?.should == true
    end
    it "should say that it is responding? if responding is not nil" do
      @ri.responding.should_not be_nil
      @ri.responding?.should == true
    end
    it "should have the puppet_runner_command" do
      @ri.respond_to?(:puppet_runner_command).should == true
    end
    it "should return the puppet_runner_command with puppetd" do
      @ri.puppet_runner_command.should =~ /puppetrerun/
    end
  end
end