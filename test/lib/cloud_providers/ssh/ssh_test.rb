require "#{File.dirname(__FILE__)}/../../../test_helper"
require fixtures_dir/'clouds/ssh_cloud.rb'
class SshProviderTest < Test::Unit::TestCase
  def cld
    @cld ||=clouds['tgarden']
  end
  
  def test_initialize_with_options_set
    assert_equal cld.cloud_provider.hosts, ["beet", 'squash']
    assert_equal  cld.cloud_provider.hosts,  cld.cloud_provider.running_hosts
    assert_not_nil cld.keypair
  end
  
  def test_responds_to_core_methods
    %w(describe_instances 
       describe_instance
       terminate_instance!
       run_instance).each do |meth|
         assert_respond_to cld.cloud_provider, meth
       end
  end
  
  def test_describe_instances
  end
  
  def test_describe_instance
  end
  
  def test_described_instances_are_sorted
  end
  
  def test_run_instances
    assert cld.cloud_provider.available_hosts.empty?
    assert_raise StandardError do
      cld.run_instance(:keypair_name => "test_key")
    end
    cld.terminate_instance!
    assert 1, cld.cloud_provider.available_hosts.size
    
    inst = cld.run_instance
    assert_kind_of CloudProviders::SshInstance, inst
    assert_equal "running", inst.status
  end
  
  def test_terminate_instances
    assert_equal "terminated", cld.cloud_provider.terminate_instance!(:name => "okra").status
  end
  
  def test_nodes
  end
  
  def test_cloud_is_set_when_created_from_a_cloud
    assert_equal cld, cld.cloud_provider.cloud
  end
  
  def test_inherited_default_options
    assert_not_nil cld.cloud_provider.cloud
    assert_nil CloudProviders::Ssh.new().cloud
  end
   
end
