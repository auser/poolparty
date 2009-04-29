require File.dirname(__FILE__) + '/../../spec_helper'
# include Remote
require File.dirname(__FILE__) + '/ec2_mocks_and_stubs.rb'

describe "ec2 remote base" do
  before(:each) do
    @cloud = TestCloud.new :test_remoter_base_cloud
    @tr = TestEC2Class.new(@cloud)
    stub_remoter_for(@tr)
    # @tr.stub!(:get_instances_description).and_return response_list_of_instances
  end
  %w(launch_new_instance! terminate_instance! describe_instance describe_instances create_snapshot).each do |method|
    eval <<-EOE
      it "should have the method #{method}" do
        @tr.respond_to?(:#{method}).should == true
      end
    EOE
  end
  describe "helpers" do
    it "should be able to convert an ec2 ip to a real ip" do
      "ec2-72-44-36-12.compute-1.amazonaws.com".convert_from_ec2_to_ip.should == "72.44.36.12"
    end
    it "should not throw an error if another string is returned" do
      "72.44.36.12".convert_from_ec2_to_ip.should == "72.44.36.12"
    end
    it "should be able to parse the date from the timestamp" do
      "2008-11-13T09:33:09+0000".parse_datetime.should == DateTime.parse("2008-11-13T09:33:09+0000")
    end
    it "should rescue itself and just return the string if it fails" do
      "thisisthedate".parse_datetime.should == "thisisthedate"
    end
  end
  describe "launching" do
    before(:each) do
      @ret_hash = {:instance_id => "1", :name => "instance"}
      @tr.ec2({}).stub!(:run_instances).and_return @ret_hash
    end
    it "should call run_instances on the ec2 Base class when asking to launch_new_instance!" do
      # @tr.ec2.should_receive(:run_instances).and_return true
      @tr.launch_new_instance!
    end
    it "should use a specific security group if one is specified" do
      @tr.stub!(:security_group).and_return "web"
      @tr.ec2.should_receive(:run_instances).and_return @ret_hash
      @tr.launch_new_instance!      
    end
    # it "should use the default security group if none is specified" do
    #   @tr.ec2.should_receive(:run_instances).with(hash_including(:group_id => ['default'])).and_return @ret_hash
    #   @tr.launch_new_instance!
    # end
    it "should get the hash response from EC2ResponseObject" do
      EC2ResponseObject.should_receive(:get_hash_from_response).and_return @ret_hash
      @tr.launch_new_instance! :keypair => "keys"
    end
  end
  describe "terminating" do
    it "should call terminate_instance! on ec2 when asking to terminate_instance!" do
      @tr.ec2.should_receive(:terminate_instances).with(:instance_id => "abc-123").and_return true
      @tr.terminate_instance!({:instance_id => "abc-123"})
    end
  end
  describe "describe_instance" do
    it "should return a default instance if called with no paramters" do
      @tr.describe_instances(:id => "i-1234").size.should > 0
    end
    it "should return nil if the cloud has no instances" do
      @tr.stub!(:describe_instances).and_return []
      @tr.describe_instance.nil?.should == true
    end
  end
  describe "get_instances_description" do  #NOTE MF: this test is sorta bogus since it is just checking what we stubbed 
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
      @tr.describe_instances[2][:name].should == "node2"
    end
  end
  describe "elastic_ips" do
    before(:each) do
      @resp = {"requestId"=>"be9bd2e9-4f8c-448f-993d-c21fe537e322", "addressesSet"=>{"item"=>[{"instanceId"=>nil, "publicIp"=>"174.129.212.93"}, {"instanceId"=>nil, "publicIp"=>"182.199.200.201"}]}, "xmlns"=>"http://ec2.amazonaws.com/doc/2008-12-01/"}
      @tr.ec2.stub!(:describe_addresses).and_return @resp
    end
    it "should have the next available elastic_ip" do
      @tr.next_unused_elastic_ip.should == "174.129.212.93"
    end
    it "should use only the elastic ips set on the cloud" do
      @cloud.stub!(:elastic_ips?).and_return true
      @cloud.stub!(:elastic_ips).and_return ["182.199.200.201"]
      @tr.stub!(:cloud).and_return @cloud
      @tr.next_unused_elastic_ip.should == "182.199.200.201"
    end
  end
  describe "create_keypair" do
    before(:each) do
      Kernel.stub!(:system).with("ec2-add-keypair fake_keypair > #{Default.base_keypair_path}/id_rsa-fake_keypair && chmod 600 #{Default.base_keypair_path}/id_rsa-fake_keypair").and_return true
      # @tr.stub!(:base_keypair_path).and_return "#{ENV["HOME"]}/.ec2"
    end
    it "should try to create the directory when making a new keypair" do
      # FileUtils.should_receive(:mkdir_p).and_return true
      # ::File.stub!(:directory?).and_return false
      # @tr.create_keypair
      pending
      #TODO Fix with new remoter branch
    end
    it "should not create a keypair if the keypair is nil" do
      pending
      #TODO Fix with new remoter branch
      # Kernel.should_not_receive(:system)
      # @tr.stub!(:keypair).and_return nil
      # @tr.create_keypair
    end
  end
  describe "create_snapshot" do
    # We can assume that create_snapshot on the ec2 gem works
    before(:each) do
      @tr.ec2.stub!(:create_snapshot).and_return nil
    end
    it "should create a snapshot of the current EBS volume" do
      @tr.ec2.stub!(:create_snapshot).and_return {{"snapshotId" => "snap-123"}}
      @tr.stub!(:ebs_volume_id).and_return "vol-123"
      @tr.create_snapshot.should == {"snapshotId" => "snap-123"}
    end
    it "should not create a snapshot if there is no EBS volume" do
      @tr.create_snapshot.should == nil
    end
  end
end
