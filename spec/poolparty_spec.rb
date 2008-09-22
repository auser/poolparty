require File.dirname(__FILE__) + '/spec_helper'

describe "PoolParty" do
  it "should have the method copy_file_to_storage_directory on the PoolParty" do
    PoolParty.respond_to?(:copy_file_to_storage_directory).should == true
  end
  it "should copy the file given with File" do
    FileUtils.should_receive(:cp).with("haymaker", Base.storage_directory).once
    PoolParty.copy_file_to_storage_directory("haymaker")
  end
end