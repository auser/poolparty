require "#{File.dirname(__FILE__)}/../../test_helper"

class CloudTest < Test::Unit::TestCase

  context "load_from_file" do
    setup do
      clear!
      @filepath = fixtures_dir/"clouds/simple_cloud.rb"
      @pool = PoolParty::Pool.load_from_file(@filepath)
      @cloud = @pool.clouds[@pool.clouds.keys.first]
    end
    
    should "be able to set the dependency_resolver" do
      @cloud.dependency_resolver :chef
      assert_equal @cloud.dependency_resolver, DependencyResolvers::Chef
    end
    
    should "raise an error if you cannot resolve_with the dependency resolver" do
      PoolParty::PoolPartyError.create("DependencyResolverError")
      assert_raises DependencyResolverError do
        @cloud.resolve_with :foo
      end
    end
    
    should "not raise an error if the dependency resolver exists" do
      assert_nothing_raised do
        @cloud.resolve_with :chef
      end
    end
    
    should "have a pool name" do
      assert_equal "poolparty", @cloud.pool.name
    end
    
    should "have a keypair" do
      assert_not_nil clouds['app'].keypair
      assert_equal 'test_key', clouds['app'].keypair.basename
    end
        
    should "have a temp path of the name: Default.tmp_path / pool_name / cloud_name" do
      assert_equal PoolParty::Default.tmp_path/"poolparty"/"app", @cloud.tmp_path
    end
    
    should "be using ec2 cloud_provider by default" do
      assert_equal :ec2, clouds['app'].cloud_provider_name
      assert_kind_of ::CloudProviders::Ec2, clouds['app'].cloud_provider
    end
    
    should "raise if the cloud_provider is not a known type" do
      PoolParty::PoolPartyError.create("UnknownCloudProviderError")
      assert_raises UnknownCloudProviderError do
        clouds["app"].cloud_provider_name = :not_a_cloud_provider
        clouds["app"].cloud_provider
      end
    end
    
    should "set the cloud_provider cloud and keypair with cloud_provider" do
      assert_equal clouds["app"], clouds["app"].cloud_provider.cloud
      assert_equal clouds["app"].keypair.to_s, clouds["app"].cloud_provider.keypair_name
    end
    
    should "set the cloud provider with a using block" do
      clouds["app"].instance_eval do
        using :ec2 do
          image_id 'emi-39921602'
        end
      end
      assert_equal :ec2, clouds["app"].cloud_provider_name
      assert_equal CloudProviders::Ec2, clouds["app"].cloud_provider.class
      assert_equal "emi-39921602", clouds["app"].cloud_provider.image_id
    end
    
    should "compile with the dependency resolver"
      # @cloud.compile
    # end
    
  end
  
end