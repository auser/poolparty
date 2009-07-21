require "#{File.dirname(__FILE__)}/../../test_helper"

class CloudTest < Test::Unit::TestCase
  include PoolParty
  context "load_from_file" do
    setup do
      @filepath = fixtures_dir/"clouds/simple_cloud.rb"
      @pool = Pool.load_from_file(@filepath)
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
    
    should "have a temp path of the name: Default.tmp_path / pool_name / cloud_name" do
      assert_equal Default.tmp_path/"poolparty"/"app", @cloud.tmp_path
    end
    
    should "compile with the dependency resolver"
      # @cloud.compile
    # end
    
  end
  
end