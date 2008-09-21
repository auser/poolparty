require File.dirname(__FILE__) + '/../spec_helper'

describe "Base" do
  it "should set the environment, if not set to production" do
    Base.environment.should == "production"
  end
  it "should set the user to poolparty" do
    Base.user.should == "poolparty"
  end
  it "should set the base keypair path to ~/.ec2" do
    Base.base_keypair_path.should == "~/.ec2"
  end
end