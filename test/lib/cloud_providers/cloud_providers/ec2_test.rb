require "#{File.dirname(__FILE__)}/../../../test_helper"

PoolParty::Keypair.searchable_paths << fixtures_dir/"keys"

require fixtures_dir/'clouds/fake_clouds'
require 'fakeweb'

FakeWeb.register_uri(:get, /.*Action=DescribeInstances.*/,
                     # :body => open(fixtures_dir/"ec2/ec-describe-instances_response_body.xml").read,
                     :body => 'stuff',
                     :status => ["200", "OK"])

# FakeWeb.register_uri(:get, /NOT-AWSAccessKeyId=fake_access_key&Action=DescribeInstances&.*/, :body=>'instance_list')

class Ec2ProviderTest < Test::Unit::TestCase
  
  def setup
    @provider = CloudProviders::Ec2.new(:image_id => "ami-abc123")
  end
  
  def test_setup
    assert_not_nil clouds['app']
    assert_not_nil clouds['app'].keypair
  end
  
  
  def test_initialize_with_options_set
    inst = CloudProviders::Ec2.new :image_id => "ami-abc123"
    assert_equal inst.image_id, "ami-abc123"
    assert_nil inst.keypair_name
  end
  
  def test_responds_to_core_methods
    %w(describe_instances 
       describe_instance
       terminate_instance!
       run_instance).each do |meth|
         assert_respond_to @provider, meth
       end
  end
  
  def test_describe_instances
    assert_respond_to @provider, :describe_instances
    assert_equal [], @provider.describe_instances
  end
  
  def test_basic_setup
    assert_equal :ec2, clouds['app'].cloud_provider_name
    assert_instance_of CloudProviders::Ec2, clouds['app'].cloud_provider
    assert_instance_of RightAws::Ec2, clouds['app'].cloud_provider.ec2
  end
  
  def test_that_test_ec2_env_variables_are_set
    assert_equal 'fake_access_key', ENV['EC2_ACCESS_KEY']
    assert_equal 'fake_secret_key', ENV['EC2_SECRET_KEY']
  end
  
  def test_default_access_key
    assert_equal 'fake_access_key', @provider.access_key
    assert_equal 'new_key', @provider.access_key('new_key')
    assert_equal 'new_new_key', @provider.access_key='new_new_key'
  end
  
  # def test_bundle_instance
  #   assert @cld.responds_to?(:bundle)
  # end
end
