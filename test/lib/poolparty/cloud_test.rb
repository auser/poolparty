require "#{File.dirname(__FILE__)}/../../test_helper"
# require 'rr'
stub_ec2_calls

class CloudTest < Test::Unit::TestCase
  # include RR::Adapters::TestUnit
  def setup
    clear!
    @filepath = fixtures_dir/"clouds/simple_cloud.rb"
    @pool = PoolParty::Pool.load_from_file(@filepath)
    @cloud = @pool.clouds[@pool.clouds.keys.first]
  end
    
  def test_be_able_to_set_the_dependency_resolver
    @cloud.dependency_resolver :chef
    assert_equal @cloud.dependency_resolver, DependencyResolvers::Chef
  end
  
  def test_raise_an_error_if_you_cannot_resolve_with_the_dependency_resolver
    PoolParty::PoolPartyError.create("DependencyResolverError")
    assert_raises DependencyResolverError do
      @cloud.resolve_with :foo
    end
  end
  
  def test_not_raise_an_error_if_the_dependency_resolver_exists
    assert_nothing_raised do
      @cloud.resolve_with :chef
    end
  end
  
  def test_have_a_pool_name
    assert_equal "poolparty", @cloud.pool.name
  end
  
  def test_have_a_keypair
    assert_not_nil clouds['app'].keypair
    assert_equal 'test_key', clouds['app'].keypair.basename
  end
  
  def test_set_the_dependency_resolver
    clouds['app'].dependency_resolver(:chef)
    assert_equal DependencyResolvers::Chef, clouds['app'].dependency_resolver
  end
  
  def test_can_use_basic_resources
    clouds['app'].instance_eval do
      has_file "/etc/motd"
    end
    assert_equal "/etc/motd", clouds['app'].files.first.name
  end
  
  def test_have_a_temp_path_of_the_name_as_Default_tmp_path_pool_name_cloud_name
    assert_equal PoolParty::Default.tmp_path/"poolparty"/"app", @cloud.tmp_path
  end
  
  def test_be_using_ec2_cloud_provider_by_default
    assert_equal :ec2, clouds['app'].cloud_provider_name
    assert_kind_of ::CloudProviders::Ec2, clouds['app'].cloud_provider
  end
  
  def test_raise_if_the_cloud_provider_is_not_a_known_type
    PoolParty::PoolPartyError.create("UnknownCloudProviderError")
    assert_raises UnknownCloudProviderError do
      clouds["app"].cloud_provider_name = :not_a_cloud_provider
      clouds["app"].cloud_provider
    end
  end
    
  def test_set_the_cloud_provider_cloud_and_keypair_with_cloud_provider
    assert_equal clouds["app"], clouds["app"].cloud_provider.cloud
    assert_equal clouds["app"].keypair.basename, clouds["app"].cloud_provider.keypair_name
  end
  
  def test_set_the_cloud_provider_with_a_using_block
    clouds["app"].instance_eval do
      using :ec2 do
        image_id 'emi-39921602'
      end
    end
    assert_equal :ec2, clouds["app"].cloud_provider_name
    assert_equal CloudProviders::Ec2, clouds["app"].cloud_provider.class
    assert_equal "emi-39921602", clouds["app"].cloud_provider.image_id
  end
  
  def test_nodes
    assert_respond_to clouds['app'], :nodes
    assert_respond_to clouds['app'].nodes, :each
    assert clouds['app'].nodes.size>1
  end
  
  def test_terminate!
    assert clouds['app'].nodes.size > 0
    result = clouds['app'].terminate!
    assert_respond_to result, :each
    assert_equal 'shutting-down', result.first.status
  end
  
  def test_run
    result = clouds['app'].run('uptime')
    assert_match /uptime/, result["app"]
  end
  
  def test_os
    assert_equal :centos, clouds['app'].os
  end
  
  def test_expansion
    #TODO: improve this test
    # size = clouds["app"].nodes.size
    # assert_equal size+1, clouds["app"].expand.nodes.size
    # assert_nothing_raised clouds['app'].expand
  end
  
  def test_contract!
    #TODO: need to better mock the terminate! ec2 call
    # size = clouds['app'].nodes.size
    # result = clouds['app'].contract!
    # assert_equal 'shuttin-down',  result.status
    # assert_equal size-1, clouds['app'].nodes.size
  end
  
  def test_monitor_dsl
    clear!
    pool "monitoring" do
      cloud "monitor_app" do
        monitor :cpu do |v|
          configure if v < 0.2
          vote_for(:expand) if v > 1.1
        end
      end
    end
    
    assert_equal 1, clouds["monitor_app"].monitors.size
    assert_equal [:cpu], clouds["monitor_app"].monitors.map {|m,v| v.name }
    assert_equal({:configure => []}, clouds["monitor_app"].run_monitor("cpu", "0.1"))
    assert_equal({:vote_for => [:expand]}, clouds["monitor_app"].run_monitor("cpu", "1.4"))
  end
end