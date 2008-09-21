require File.dirname(__FILE__) + '/../spec_helper'

include Remote

class TestClass
  include CloudResourcer
  include Remoter
  
  def keypair
    "fake_keypair"
  end
end
describe "Remoter" do
  before(:each) do
    @tc = TestClass.new
  end
  describe "ssh_string" do
    it "should have the ssh command" do
      @tc.ssh_string.should =~ /ssh -o StrictHostKeyChecking=no -l '#{Base.user}' -i/
    end
    it "should have the keypair in the ssh_string" do
      @tc.ssh_string.should =~ /#{@tc.keypair}/
    end
  end
  describe "ssh_array" do
    it "should have StrictHostKeyChecking set to no" do
      @tc.ssh_array.include?("-o StrictHostKeyChecking=no").should == true
    end
    it "should have the user set to the base user class" do
      @tc.ssh_array.include?("-l '#{Base.user}'").should == true
    end
    it "should have the keypair path in the ssh_array" do
      @tc.ssh_array.include?("-i '#{@tc.keypair_path}'").should == true
    end
  end
  describe "rsync_command" do
    it "should have rsync in the rsync_command" do
      @tc.rsync_command.should == "rsync --delete -azP -e '#{@tc.ssh_string}' "
    end
  end
end