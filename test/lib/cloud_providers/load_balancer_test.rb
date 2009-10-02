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
  
  def test_from_within_a_cloud_setup
    clear!
    pool :load_balancer_test_cloud do
      cloud :elb do
        load_balancer "TestLoadBalancer" do
          protocol "tcp"
        end
        load_balancer "TestLoadBalancer2" do
          protocol "http"
        end
        load_balancer "TestLoadBalancer3" do
          protocol "http"
        end
      end
    end
    
    cld = clouds["elb"]
    
    assert_equal 3, (cld.send :_load_balancers_args).size
    assert_equal 3, cld.load_balancers.size
    assert_equal "http", cld.load_balancers[0].protocol
    assert_equal "http", cld.load_balancers[1].protocol
    p cld.load_balancers[1]
  end
    
end