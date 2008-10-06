require File.dirname(__FILE__) + '/spec_helper'

describe "PoolParty" do
  after(:all) do
    # FileUtils.rm_r(Base.storage_directory) rescue ""
  end
  it "should have the method copy_file_to_storage_directory on the PoolParty" do
    PoolParty.respond_to?(:copy_file_to_storage_directory).should == true
  end
  it "should copy the file given with File" do
    FileUtils.should_receive(:cp).with("haymaker", Base.storage_directory + "/haymaker").once
    PoolParty.copy_file_to_storage_directory("haymaker")
  end
  describe "writing file to storage_directory" do
    before(:each) do
      @path = "#{Base.storage_directory}/happydayz"
    end
    it "should have the method write_to_file_in_storage_directory" do
      PoolParty.respond_to?(:write_to_file_in_storage_directory).should == true
    end
    it "should receive File.new with the name of the file" do
      File.should_receive(:open).with(@path, "w+").once.and_return true
      PoolParty.write_to_file_in_storage_directory("/usr/bin/happydayz", "write this text")
    end
    it "should write the contents of the file to the file" do
      PoolParty.write_to_file_in_storage_directory("/usr/bin/happydayz", "write this text")
      File.open(@path).read.should == "write this text"
    end
  end
  it "should have a logger" do
    PoolParty.logger.should_not be_nil
  end
end