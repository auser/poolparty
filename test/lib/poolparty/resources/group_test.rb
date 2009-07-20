require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class GroupResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Group.new "moderators"
      @base = PoolParty::DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the group method denoted by has_method_name" do
      str = "group \"moderators\" do\n   action :create\nend"
      assert_equal str, @base.compile(@res)
    end
    
  end
  
end