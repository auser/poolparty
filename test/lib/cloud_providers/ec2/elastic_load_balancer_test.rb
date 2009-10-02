require "#{File.dirname(__FILE__)}/../../../test_helper"

stub_ec2_calls

class LoadBalancerTest < Test::Unit::TestCase
  
  def setup
    @filepath = fixtures_dir/"clouds/simple_cloud.rb"
    @pool = PoolParty::Pool.load_from_file(@filepath)
    @cloud = @pool.clouds[@pool.clouds.keys.first]
    @cloud_provider = @cloud.cloud_provider
    
    @lb = CloudProviders::LoadBalancer.new "franklin" do
      protocol "tcp"
    end
  end

  def ec2
    @ec2 ||= @cloud.cloud_provider.send :grempe_elb
  end
  
  def test_do_something
    assert true
  end
    
end