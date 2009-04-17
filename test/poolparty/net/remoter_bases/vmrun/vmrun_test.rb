require "#{::File.dirname(__FILE__)}/../../../../test_helper"

class TestVmRun < Test::Unit::TestCase
  context "registered as remote base" do
    setup do
      reset!
      @cloud = cloud :test_vm_runner do
        
        using :vmrun do
          network_addresses '00:0c:29:55:f1:0f'=> '10.45.10.185'
        end
        
      end
    end
    
    should "have the network_addresses set from the block" do
      @cloud.vmrun.network_addresses.should == {'00:0c:29:55:f1:0f'=> '10.45.10.185'}
    end
    
  end
  
end