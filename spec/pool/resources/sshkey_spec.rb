require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "SshKey" do
  before(:each) do
    @sshkey = PoolParty::Resources::SshKey.new
  end
  it "should have instances of files" do
    @sshkey.respond_to?(:instances).should == true
  end
  describe "instances" do
    before(:each) do
      @instance1 = {:name => "rock"}
      @sshkey << @instance1
    end
    it "should turn the one hash instance into a string" do
      @sshkey.to_s.should =~ /rock:\n/
    end
    it "should turn the two hash instance into a string" do
      @instance2 = {:name => "poolparty"}
      @sshkey << @instance2
      @sshkey.to_s.should =~ /poolparty:/
    end
    describe "file" do
      before(:each) do
        @string = "ALONGSTRINGOFDIGITS"
        @file = File.join(File.dirname(__FILE__), "..", "test_plugins", "sshkey_test")
        @string.stub!(:read).and_return @string
      end
      it "should read the file when sent keyfile=" do
        @sshkey.should_receive(:open).and_return @string
        @sshkey.keyfile = @file
      end
      it "should set the keyfile as the contents of the file" do
        @sshkey.keyfile = @file
        @sshkey.keyfile.should =~ /THIS IS A TEST/
      end
    end
  end
end
