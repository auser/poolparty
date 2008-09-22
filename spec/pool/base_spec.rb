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
  it "should set the storage_directory to the tmp directory of the current working directory" do
    Base.storage_directory.should == "#{Dir.pwd}/tmp"
  end
  it "should set the tmp path to tmp" do
    Base.tmp_path.should == "tmp"
  end
  it "should set the remote storage path to /var/poolparty" do
    Base.remote_storage_path.should == "/var/poolparty"
  end
  it "should set the fileserver_base to puppet://puppet/" do
    Base.fileserver_base.should == "puppet://puppet/"
  end
end