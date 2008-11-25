require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Remote

def valid_rules?(*args)
  false
end

class TestClass
  include CloudResourcer
  include Remote
  using :ec2
  attr_accessor :parent
    
  def keypair
    "fake_keypair"
  end
end
describe "Remoter" do
  before(:each) do
    setup
    @cloud = cloud :app do;end
    @tc = TestClass.new
    @tc.parent = @cloud
    ::File.stub!(:exists?).with("#{File.expand_path(Base.base_keypair_path)}/id_rsa-fake_keypair").and_return true
    @sample_instances_list = [{:ip => "192.168.0.1", :name => "master"}, {:ip => "192.168.0.2", :name => "node1"}]
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
      @tc.ssh_array.include?('-i "'+@tc.full_keypair_path+'"').should == true
    end
  end
  describe "rsync_command" do
    before(:each) do
      @ri = Class.new
      @ri.stub!(:ip).and_return "192.168.0.22"
    end
    it "should have rsync in the rsync_command" do
      @tc.rsync_command.should == "rsync -azP --exclude cache -e '#{@tc.ssh_string}'"
    end
    it "should be able to rsync storage commands" do
      @tc.rsync_storage_files_to_command(@ri).should == "#{@tc.rsync_command} #{Base.storage_directory}/ 192.168.0.22:/var/poolparty"
    end
  end
  describe "launch_and_configure_master!" do
    before(:each) do
      @tc.stub!(:wait).and_return true
      stub_list_from_remote_for(@tc)
      @tc.stub!(:maximum_instances).and_return 5
      @tc.stub!(:list_of_pending_instances).and_return []
      @tc.stub!(:list_of_nonterminated_instances).and_return []
      @tc.stub!(:list_of_running_instances).and_return []
      @tc.stub!(:master).and_return ris.first
      @tc.stub!(:after_launched).and_return true
      @tc.stub!(:verbose).and_return false
      Provisioner.stub!(:provision_master).and_return true
      Provisioner.stub!(:reconfigure_master).and_return true
      Provisioner.stub!(:clear_master_ssl_certs).and_return true
    end
    it "should have the method launch_master!" do
      @tc.respond_to?(:launch_and_configure_master!).should == true
    end
    it "should test if it can start a new instance" do
      @tc.should_receive(:can_start_a_new_instance?).once.and_return false      
    end
    it "should test if the master is running" do
      @tc.should_receive(:is_master_running?).and_return false
    end
    it "should ask to request_launch_new_instances when the master is not running and we can start a new instance" do
      @tc.should_receive(:request_launch_master_instance)
      @tc.stub!(:can_start_a_new_instance?).and_return true
      @tc.stub!(:is_master_running?).and_return false
    end
    it "should tell the provisioner to provision_master" do
      Provisioner.should_receive(:provision_master).once.and_return true
    end
    after(:each) do
      @tc.launch_and_configure_master!
    end
  end

end