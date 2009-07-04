require "#{File.dirname(__FILE__)}/../../test_helper"

class ChefTest < Test::Unit::TestCase
  context "chef dependency_resolver test" do
    setup do
      @base = PoolParty::DependencyResolvers::Chef
    end
    
    should "have compile to chef" do
      assert @base.respond_to?(:compile_method_name)
      assert_equal :print_to_chef, @base.compile_method_name
    end
  end
  
end