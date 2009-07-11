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
    
    should "compile with the dependency resolver"
      # @cloud.compile
    # end
    
  end
  
end