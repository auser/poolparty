require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "SshKey" do
  before(:each) do
    @sshkey = PoolParty::Resources::Sshkey.new
  end
  it "should have instances of files" do
    @sshkey.respond_to?(:instances).should == true
  end
  describe "instances" do
    before(:each) do
      reset_resources!      
    end
    it "should turn the one hash instance into a string" do
      sshkey({:name => "rock"})
      sshkey.to_string.should =~ /rock:\n/
    end
    it "should turn the two hash instance into a string" do
      sshkey do
        name "poolparty_key"
      end
      sshkey.to_string.should =~ /poolparty_key:/
    end
    describe "sizes" do
      before(:each) do
        sshkey({:name => "rock"})
        sshkey({:name => "dos"})
      end
      it "should contain two keyfiles if two are specified" do      
        sshkey.instances.size.should == 3
      end
    end
    describe "file" do
      before(:each) do
        reset_resources!
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
