require "#{File.dirname(__FILE__)}/../../test_helper"

stub_ec2_calls

class AutoScalingTest < Test::Unit::TestCase
  
  def setup
    @filepath = fixtures_dir/"clouds/simple_cloud.rb"
    @pool = PoolParty::Pool.load_from_file(@filepath)
    @cloud = @pool.clouds[@pool.clouds.keys.first]
    
    @as = CloudProviders::AutoScaling.new "scaling in the east" do
    end
  end
  
  def test_load_balancer_instantiation
    assert_equal @as.class, CloudProviders::AutoScaling
  end
    
end