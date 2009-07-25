require "#{File.dirname(__FILE__)}/../../../test_helper"

require fixtures_dir/'clouds/vmware_cloud'

class CloudProviders::Vmware
  def self.stubbed_output
    @stubbed_output ||= {}
  end
  def vmrun(cmd, o={})
    self.class.stubbed_output[cmd]
  end
end

class VmwareProviderTest < Test::Unit::TestCase
  
  def setup
    @provider = CloudProviders::Vmware.new(
                  :image_id => "testfile.vmx", 
                  :keypair => fixtures_dir/'keys/test_key'
                )
  end
  
  def test_vmrun_list
    stub_vmrun_call("list", ["Total running VMs: 0"])
    assert_equal [], @provider.describe_instances
    stub_vmrun_call("list", ["Total running VMs: 1", "#{ENV["HOME"]}/Documents/Virtual\ Machines\.localized/ubuntu.vmwarevm/ubuntu.vmx"])
    inst = @provider.describe_instances.first
    assert_equal CloudProviders::VmwareInstance, inst.class
    assert_equal "#{ENV["HOME"]}/Documents/Virtual\ Machines\.localized/ubuntu.vmwarevm/ubuntu.vmx", inst.instance_id
  end
  
  def test_run_instances
    vmx_path = "#{ENV["HOME"]}/Documents/Virtual\ Machines\.localized/ubuntu.vmwarevm/ubuntu.vmx"
    stub_vmrun_call("list", ["Total running VMs: 0"])
    stub_vmrun_call("run #{vmx_path}", [])
    assert_equal CloudProviders::VmwareInstance, @provider.run_instance(:image_id => vmx_path).class
  end
    
    
  private
  
  def stub_vmrun_call(cmd, output)
    CloudProviders::Vmware.stubbed_output[cmd] = output
  end
  
end
