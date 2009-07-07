require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class DirectoryResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Directory.new "/etc/poolparty"
      @base = PoolParty::DependencyResolvers::Chef
    end
    
    should "have the method denoted by has_method_name" do
      str = 'directory "/etc/poolparty" do
  recursive: "true"
mode: "0644"
name: "/etc/poolparty"
end
'
      assert_match /directory "\/etc\/poolparty"/, @res.compile(:chef)
      assert_match /recursive: "true"/, @res.compile(:chef)
      assert_match /mode: "0644"/, @res.compile(:chef)
      assert_match /name: "\/etc\/poolparty"/, @res.compile(:chef)
    end
    
  end
  
end