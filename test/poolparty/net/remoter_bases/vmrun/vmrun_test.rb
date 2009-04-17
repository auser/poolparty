require "#{::File.dirname(__FILE__)}/../../../../test_helper"

class TestVmRun < Test::Unit::TestCase
  context "registered as remote base" do
    setup do
      reset!
      $vmx_file = "/Users/alerner/Documents/Virtual Machines.localized/Ubuntu Linux 32bit.vmwarevm/Ubuntu Linux 32bit.vmx"
      @cloud = cloud :test_vm_runner do
        
        using :vmrun do
          vmx_file $vmx_file
        end
        
      end      
    end    
    should "be setting the type of remote_base as Vmrun" do
      @cloud.remote_base.class.should == PoolParty::Remote::Vmrun
    end
    should "have the method vmrun as the remoter base" do
      @cloud.vmrun.should == @cloud.remote_base
    end    
    should "start vmware instance" do
      @cloud.launch_instance!
      assert @cloud.vmrun.describe_instances.include?($vmx_file)
    end
    should "have the ip and mac address" do
      @cloud.launch_instance!
      
      id = @cloud.vmrun.describe_instances.first
      instance = @cloud.vmrun.describe_instance(:vmx_file => id)
      assert !instance.mac_address.nil?
      assert !instance.ip.nil?
    end    
    should "be able to turn off the instance" do
      @cloud.launch_instance!
      @cloud.terminate_instance!
      assert @cloud.vmrun.describe_instances.empty?
    end
    after :all do
      PoolParty::Remote::Vmrun.terminate! @cloud.vmrun.options
    end
  end
  
end