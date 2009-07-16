require "#{File.dirname(__FILE__)}/../../test_helper"

class CloudTest < Test::Unit::TestCase
  include PoolParty
  context "load_from_file" do
    setup do
      @filepath = File.join(File.dirname(__FILE__), "../../../", "examples/simple.rb")
      @pool = Pool.load_from_file(@filepath)
      @cloud = @pool.clouds[@pool.clouds.keys.first]
    end

    should "be able to set the dependency_resolver" do
      @cloud.dependency_resolver :chef
      assert_equal @cloud.dependency_resolver, PoolParty::DependencyResolvers::Chef
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
    
    should "compile with the dependency resolver"
      # @cloud.compile
    # end
    
  end
  
end