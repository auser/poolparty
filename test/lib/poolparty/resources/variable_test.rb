require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class VariableResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Variable.new "files", ["a_file", "b_file"]
      @base = PoolParty::DependencyResolvers::Chef
    end
    
    should "add the name and value as options on the variable instance" do
      assert_equal @res.name, "files"
      assert_equal @res.value, ["a_file", "b_file"]
    end
    
  end
  
end