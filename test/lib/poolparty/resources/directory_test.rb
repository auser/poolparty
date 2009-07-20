require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class DirectoryResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Directory.new "/etc/poolparty"
      @base = PoolParty::DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the method denoted by has_method_name" do
      str = "directory \"/etc/poolparty\" do\n  action :create\n  recursive false\n  mode 0644\n  owner \"root\"\n  group \"root\"\nend\n"
      assert_equal str, @base.compile(@res)
    end
    
  end
  
end