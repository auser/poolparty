require "#{File.dirname(__FILE__)}/../../../test_helper"
require File.dirname(__FILE__)+"/ec2_test.rb"

stub_keypair_searchable_paths

class Ec2InstanceTest < Test::Unit::TestCase
  include CloudProviders
  
  def setup
    clear!
    @filepath = fixtures_dir/"clouds/simple_cloud.rb"
    @pool = PoolParty::Pool.load_from_file(@filepath)
    @cloud = @pool.clouds[@pool.clouds.keys.first]
  end
  
  def inst
    @inst ||= @cloud.describe_instances.first
  end
  
  def test_has_cloud_provider
    inst = CloudProviders::Ec2.new(:image_id => "ami-abc123").describe_instances.first
    assert_kind_of CloudProviders::Ec2, inst.cloud_provider
  end
  
  def test_hosts_file_listing_for
    #TODO
  end
  
  def test_to_s
    vals = inst.to_s.split("\t")
    assert_equal 3, vals.size
    assert_equal 'simple_cloud', vals.first
  end
  
  def test_has_cloud_set_when_created_from_cloud
    assert_equal @cloud, @cloud.cloud_provider.cloud
    assert_equal @cloud, inst.cloud
    assert_equal 'simple_cloud', inst.dsl_options[:cloud_name]
    assert_equal 'simple_cloud', inst.to_hash[:cloud_name]
  end
  
  def test_cloud_keypair
    assert_equal  @cloud.keypair.to_s,  inst.keypair.to_s
  end

  def test_refresh!
    inst.status = 'testing'
    inst.public_ip = 'notanip'
    assert_equal 'testing',  inst.status
    assert_equal 'notanip', inst.public_ip
    inst.refresh!
    assert_equal '75.101.141.103', inst.public_ip
    assert_equal 'running', inst.status
  end
  
  def test_terminate!
    zombie =  inst.terminate!
    assert_equal 'shutting-down', zombie.status
    assert_kind_of Ec2Instance, zombie
  end
  
  
  #TODO: this needs better tests
  def test_wait_for_public_ip
    assert inst.wait_for_public_ip
  end
  
end