require "#{File.dirname(__FILE__)}/../../../../test_helper"

include_fixture_resources
include_chef_only_resources

class RemoteDirectoryResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::RemoteDirectory.new "/tmp/remote_something" do
        source "something"
        files_backup 10
        files_owner "root"
        files_group "root"
        files_mode "0644"
        owner "nobody"
        group "nobody"
        mode "0755"
      end
      @base = DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the method denoted by has_method_name for remote_directory" do
      str =<<-EOE
remote_directory "/tmp/remote_something" do
  source "something"
  files_backup 10
  files_mode "0644"
  action :create
  recursive false
  mode "0755"
  owner "nobody"
  group "nobody"
  files_owner "root"
  files_group "root"
end
EOE

      assert_equal str.chomp, @base.compile(@res)
    end
    
  end
  
end