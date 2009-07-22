require "#{File.dirname(__FILE__)}/../../../test_helper"
require File.dirname(__FILE__)+"/ec2_test.rb"

class Ec2InstanceTest < Test::Unit::TestCase
  include CloudProviders

  def setup
    @provider = CloudProviders::Ec2.new(:image_id => "ami-abc123")
  end
  
  def test_initialize
    #TODO
  end
  
  def test_has_cloud_provider
    inst = @provider.describe_instances.first
    assert_kind_of CloudProviders::Ec2, inst.cloud_provider
  end
  
  def test_hosts_file_listing_for
    #TODO
  end
  
  def test_to_s
    #TODO
  end
  
end