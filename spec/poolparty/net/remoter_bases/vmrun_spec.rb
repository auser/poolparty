require File.dirname(__FILE__) + '/../../../spec_helper'
# include Remote

describe "ec2 remote base" do
  before(:each) do
    @vmx_file = "/Users/mfairchild/Documents/Virtual_Machines.localized/Ubuntu32bitVM.vmwarevm/Ubuntu32bitVM.vmx"    
    @vmrunner = Vmrun.new(:vmx_file => @vmx_file)
  end
  
  %w(launch_new_instance! terminate_instance! describe_instance describe_instances).each do |method|
    eval <<-EOE
      it "should have the method #{method}" do
        @vmrunner.respond_to?(:#{method}).should == true
      end
    EOE
  end
  
  it "should have a vmrun command available" do
    @vmrunner.path.should =~ /vmrun/
    `which #{@vmrunner.path}`.length.should > 4
    $?.success?.should == true
  end

  # describe "launching" do
  #   before(:each) do
  #     @vmrunner.vm.stub!(:run_instances).and_return true
  #   end
  #   it "should call run_instances on the ec2 Base class when asking to launch_new_instance!" do
  #     # @vmrunner.vm.should_receive(:run_instances).and_return true
  #     @vmrunner.launch_new_instance!
  #   end
  #   it "should use a specific security group if one is specified" do
  #     @vmrunner.stub!(:security_group).and_return "web"
  #     @vmrunner.vm.should_receive(:run_instances).and_return true
  #     @vmrunner.launch_new_instance!      
  #   end
  #   it "should use the default security group if none is specified" do
  #     @vmrunner.vm.should_receive(:run_instances).with(hash_including(:group_id => ['default'])).and_return true
  #     @vmrunner.launch_new_instance!      
  #   end
  #   it "should get the hash response from EC2ResponseObject" do
  #     EC2ResponseObject.should_receive(:get_hash_from_response).and_return true
  #     @vmrunner.launch_new_instance!
  #   end
  # end
  
  # describe "terminating" do
  #   it "should call terminate_instance!" do
  #     @vmrunner.vm.should_receive(:terminate_instances).with(:instance_id => "abc-123").and_return true
  #     @vmrunner.terminate_instance!({:instance_id => "abc-123"})
  #   end
  # end
  
  describe "describe_instance" do
    it "should return a default instance if called with no paramters" do
      @vmrunner.describe_instances().size.should > 0
    end
    it "should return nil if the cloud has no instances" do
      @vmrunner.describe_instance.nil?.should == true
    end
  end
  
  
  describe "get_instances_description" do
    it "should return a hash" do
      @vmrunner.describe_instances.class.should == Array
    end
    it "should call the first node master" do
      @vmrunner.describe_instances.first[:name].nil?.should == false
    end
  end
  
end
