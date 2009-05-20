require File.dirname(__FILE__) + '/../spec_helper'

describe "Key" do
  it "be able to be instantiated with a keyfile" do
    lambda {Key.new("file")}.should_not raise_error
  end
  it "should fill in the filepath as id_rsa if no filelocation is given" do
    Key.new.filepath.should == "id_rsa"
  end
  it "should provide valid to_json" do
    lambda {Key.new("file").to_hash.to_json}.should_not raise_error  end
  describe "that exists" do
    before(:each) do
      @keypair = "/var/home/id_rsa"
      ::File.stub!(:file?).with(@keypair).and_return true
      ::File.stub!(:expand_path).with(@keypair).and_return @keypair
      
      @key = Key.new(@keypair)
    end
    it "say that the key exists" do
      @key.exists?.should == true
    end    
  end
  describe "that is not a full filepath name" do
    before(:each) do
      @keypair = "sshkey_test"      
      Dir.stub!(:pwd).and_return "#{::File.dirname(__FILE__)}/test_plugins"
      
      @key = Key.new(@keypair)
    end
    it "should search in known locations for the key" do
      @key.should_receive(:search_in_known_locations).and_return nil
      @key.full_filepath
    end
    it "return the full filepath when the key exists (checking last possible Dir.pwd)" do
      @key.full_filepath.should =~ /sshkey_test/
    end
    it "should return the content of the keyfile when requested" do
      @key.content.should == "-- THIS IS A TEST SSH KEY FILE --\n\n"
    end
  end
end