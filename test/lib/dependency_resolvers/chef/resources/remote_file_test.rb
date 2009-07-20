require "#{File.dirname(__FILE__)}/../../../../test_helper"

include_fixture_resources

class RemoteFileResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::RemoteFile.new "/tmp/testfile" do
        source "http://www.example.com/tempfiles/testfile"
        mode "0644"
        checksum "08da002l" # A SHA256 (or portion thereof) of the file.
      end
      @base = PoolParty::DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the method denoted by has_method_name for remote_file" do
      str =<<-EOE
remote_file "/tmp/testfile" do
  source "http://www.example.com/tempfiles/testfile"
  action :create
  backup 5
  mode 0644
  owner "root"
  checksum "08da002l"
end
EOE

      assert_equal str.chomp, @base.compile(@res)
    end
    
  end
  
end