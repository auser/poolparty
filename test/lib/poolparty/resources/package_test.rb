require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class DirectoryResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Package.new "debian-archive-keyring", :options => "--force-yes", :version => "1.16.1-1"
      @base = DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the method denoted by has_method_name" do
      str =<<-EOE
package "debian-archive-keyring" do
  action :install
  options "--force-yes"
  version "1.16.1-1"
end
EOE

      assert_equal str.chomp, @base.compile(@res)
    end
    
  end
  
end