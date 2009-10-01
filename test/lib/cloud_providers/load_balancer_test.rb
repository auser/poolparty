require "#{File.dirname(__FILE__)}/../../test_helper"

stub_ec2_calls

class LoadBalancerTest < Test::Unit::TestCase
  
  def setup
    @filepath = fixtures_dir/"clouds/simple_cloud.rb"
    @pool = PoolParty::Pool.load_from_file(@filepath)
    @cloud = @pool.clouds[@pool.clouds.keys.first]
    
    @lb = CloudProviders::LoadBalancer.new "franklin" do
      protocol "tcp"
    end
  end
  
  def test_load_balancer_instantiation
    assert_equal @lb.class, CloudProviders::LoadBalancer
    assert_equal @lb.protocol, "tcp"
    assert_equal @lb.balancer_port, 80
  end
    
end