require File.dirname(__FILE__) + '/../../spec_helper'

# include Remote

class TestClass  
  include PoolParty::Remote::RemoterBase
  include Ec2
  include CloudResourcer
  
  def keypair
    "fake_keypair"
  end
  
  def ami;"ami-abc123";end
  def size; "small";end
  
  def ec2
    @ec2 ||= EC2::Base.new( :access_key_id => "not_an_access_key", :secret_access_key => "not_a_secret_access_key")
  end
end
describe "ec2 remote base" do
  before(:each) do
    setup
    @tr = TestClass.new
    stub_remoter_for(@tr)
    @tr.stub!(:get_instances_description).and_return response_list_of_instances
  end
  %w(launch_new_instance! terminate_instance! describe_instance describe_instances).each do |method|
    eval <<-EOE
      it "should have the method #{method}" do
        @tr.respond_to?(:#{method}).should == true
      end
    EOE
  end
  describe "launching" do
    before(:each) do
      @tr.ec2.stub!(:run_instances).and_return true
    end
    it "should call run_instances on the ec2 Base class when asking to launch_new_instance!" do
      @tr.ec2.should_receive(:run_instances).and_return true
      @tr.launch_new_instance!
    end
    it "should get the hash response from EC2ResponseObject" do
      EC2ResponseObject.should_receive(:get_hash_from_response).and_return true
      @tr.launch_new_instance!
    end
  end
  describe "terminating" do
    it "should call terminate_instance! on ec2 when asking to terminate_instance!" do
      @tr.ec2.should_receive(:terminate_instances).with(:instance_id => "abc-123").and_return true
      @tr.terminate_instance!("abc-123")
    end
  end
  describe "describe_instance" do
    it "should call get_instances_description on itself" do
      @tr.should_receive(:get_instances_description).and_return {}
      @tr.describe_instance
    end
  end
  describe "get_instances_description" do
    it "should return a hash" do
      @tr.describe_instances.class.should == Array
    end
    it "should call the first node master" do
      @tr.describe_instances.first[:name].should == "master"
    end
    it "should call the second one node1" do
      @tr.describe_instances[1][:name].should == "node1"
    end
    it "should call the third node2" do
      @tr.describe_instances[2][:name].should == "terminated_node2"
    end
  end
  describe "create_keypair" do
    before(:each) do
      Kernel.stub!(:system).with("ec2-add-keypair fake_keypair > #{Base.base_keypair_path}/id_rsa-fake_keypair && chmod 600 #{Base.base_keypair_path}/id_rsa-fake_keypair").and_return true
    end
    it "should send system to the Kernel" do
      Kernel.should_receive(:system).with("ec2-add-keypair fake_keypair > #{Base.base_keypair_path}/id_rsa-fake_keypair && chmod 600 #{Base.base_keypair_path}/id_rsa-fake_keypair").and_return true
      @tr.create_keypair
    end
    it "should try to create the directory when making a new keypair" do
      FileUtils.should_receive(:mkdir_p).and_return true
      ::File.stub!(:directory?).and_return false
      @tr.create_keypair
    end
    it "should not create a keypair if the keypair is nil" do
      Kernel.should_not_receive(:system)
      @tr.stub!(:keypair).and_return nil
      @tr.create_keypair
    end
  end
end