require "#{File.dirname(__FILE__)}/../../../../test_helper"

include_fixture_resources
include_chef_only_resources

class ChefRecipeTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::ChefRecipe.new fixtures_dir/"chef"/"recipes"/"sudo"
      @base = DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the method denoted by has_method_name for http_request" do
      assert_equal "recipe \"sudo\"", @base.compile(@res)
    end
    
  end
  
end