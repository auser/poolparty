require "#{::File.dirname(__FILE__)}/../../../../test_helper"

# Comment out this VmwareInstanceclass if you want to actually run vmware
class VmwareInstance
  def launch!
    { :status => 'running',
      :mac_address => '00:23:6c:93:7b:91',
      :ip => '10.45.10.234',
      :internal_ip => '10.45.10.234',          
      :instance_id => vmx_file,
      :keypair => keypair
    }
  end
end

# Uncomment the tests for live-testing
class TestVmRun < Test::Unit::TestCase
  context "registered as remote base" do
    setup do
      reset!
      vmx_keys1 = {
        ::File.expand_path("~/Documents/vm/Ubuntu32bitVM.vmwarevm/Ubuntu32bitVM.vmx") => "192.168.248.133"
      }
      @vmx_files = vmx_keys1.keys
      @ipaddresses = vmx_keys1.values
      
      @cloud = cloud :test_vm_runner do
        using :vmrun do
          path_to_binary '/fake/path/to/vmrun'
          vmx_hash vmx_keys1
        end
      end
      
      @cloud2 = cloud :files do
        using :vmrun do
          vmx_files ['/path/to/vmx/file', '/another/path']
        end
      end
      
    end    
    should "be setting the type of remote_base as Vmrun" do
      @cloud.remote_base.class.should == PoolParty::Remote::Vmrun
    end
    should "have the method vmrun as the remoter base" do
      @cloud.vmrun.should == @cloud.remote_base
    end
    should "have vmx_files from vmx_hash" do
      @cloud.remote_base.vmx_files.size.should == 1
      @cloud.remote_base.vmx_files.should == @vmx_files
    end
    should "have vmx_files form vmx_files option" do
      @cloud.remote_base.vmx_files == ["~/Documents/vm/Ubuntu32bitVM.vmwarevm/Ubuntu32bitVM.vmx"]
      @cloud2.remote_base.vmx_files == ['/path/to/vmx/file', '/another/path']
    end
    # should "start vmware instance" do
    #   @cloud.launch_instance!
    #   assert @cloud.vmrun.describe_instances.first[:instance_id], $vmx_files.first
    # end
    # should "have the ip and mac address" do
    #   @cloud.launch_instance!
    #   
    #   vmx_file = @cloud.vmrun.describe_instances.first[:instance_id]
    #   instance = @cloud.vmrun.describe_instance(:vmx_file => vmx_file)
    # 
    #   assert !instance.mac_address.nil?
    #   assert !instance.ip.nil?
    # end    
    # should "be able to turn off the instance" do
    #   @cloud.launch_instance!
    #   @cloud.terminate_instance!
    #   assert @cloud.describe_instances.empty?
    # end
    # teardown do
    #   # PoolParty::Remote::Vmrun.terminate!(
    #   @cloud.describe_instances.each do |inst|
    #     @cloud.terminate_instance! inst[:instance_id]
    #   end
    # end
  end
  
end
