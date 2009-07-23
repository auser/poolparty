require "#{File.dirname(__FILE__)}/../../../test_helper"

stub_ec2_calls

class Ec2ProviderTest < Test::Unit::TestCase
  
  def setup
    @provider = CloudProviders::Ec2.new(
                  :image_id => "ami-abc123", 
                  :keypair => fixtures_dir/'keys/test_key'
                )
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
    assert_instance_of RightAws::Ec2, @provider.ec2
    
    assert_respond_to @provider, :describe_instances    
    assert_equal ["i-7fd89416", "i-7f000516"], @provider.describe_instances.map {|a| a[:instance_id]}
  end
  
  def test_describe_instance
    assert_respond_to @provider, :describe_instance

    inst = @provider.describe_instance(:instance_id => "i-7fd89416")
    assert_equal "i-7fd89416", inst.instance_id
    assert_kind_of CloudProviders::Ec2Instance, inst
  end
  
  def test_described_instances_are_sorted
    assert @provider.describe_instances.size > 0
    assert @provider.describe_instances.first.launch_time < @provider.describe_instances.last.launch_time
  end
  
  def test_run_instances
    assert_respond_to @provider, :run_instance
    inst = @provider.run_instance(:keypair_name => "test_key")
    assert_kind_of CloudProviders::Ec2Instance, inst
    assert_equal "pending", inst.status
  end
  
  def test_terminate_instances
    assert_respond_to @provider, :terminate_instance!
    assert_equal ["shutting-down"], @provider.terminate_instance!(:instance_id => "i-3B3506A0").map {|a| a[:shutdown_state] }
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
  
  def test_nodes
    assert_equal ["i-7fd89416", "i-7f000516"], @provider.nodes(:status => "running").map {|a| a[:instance_id] }
    assert_equal ["i-7fd89416"], @provider.nodes(:instance_id => "i-7fd89416").map {|a| a[:instance_id] }
  end
  
  def test_cloud_is_set_when_created_from_a_cloud
    assert_equal clouds['app'], clouds['app'].cloud_provider.cloud
  end
  
  def test_inherited_default_options
    assert_respond_to CloudProviders::Ec2.new, :cloud
    assert_nil CloudProviders::Ec2.new().cloud
  end
   
  # def test_bundle_instance
  #   assert @cld.responds_to?(:bundle)
  # end
end
