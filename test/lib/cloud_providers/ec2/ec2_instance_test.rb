require "#{File.dirname(__FILE__)}/../../../test_helper"
require File.dirname(__FILE__)+"/ec2_test.rb"

class Ec2InstanceTest < Test::Unit::TestCase
  include CloudProviders

  def setup
    @provider = CloudProviders::Ec2.new(:image_id => "ami-abc123")
  end
  
  def test_has_cloud_provider
    inst = @provider.describe_instances.first
    assert_kind_of CloudProviders::Ec2, inst.cloud_provider
  end
  
  def test_hosts_file_listing_for
    #TODO
  end
  
  def test_to_s
    vals = clouds['app'].cloud_provider.describe_instances.first.to_s.split("\t")
    assert_equal 3, vals.size
    assert_equal 'app', vals.first
  end
  
  def test_has_cloud_set_when_created_from_cloud
    assert_equal clouds['app'], clouds['app'].cloud_provider.cloud
    inst = clouds['app'].cloud_provider.describe_instances.first
    assert_equal clouds['app'], inst.cloud
    assert_equal 'app', inst.dsl_options[:cloud_name]
    assert_equal 'app', inst.to_hash[:cloud_name]
  end
  
  def test_cloud_keypair
    inst = clouds['app'].cloud_provider.describe_instances.first
    assert_equal  clouds['app'].keypair.to_s,  inst.keypair.to_s
  end
  
end