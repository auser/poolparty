require "#{File.dirname(__FILE__)}/../../../../test_helper"

include_fixture_resources
include_chef_only_resources

class ScriptResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Script.new "install_something" do
        interpreter "bash"
        user "root"
        cwd "/tmp"
        code <<-EOH 
wget http://www.example.com/tarball.tar.gz
tar -zxf tarball.tar.gz
cd tarball
./configure
make
make install
EOH
      end
      @res.does_not_exist!
      @base = PoolParty::DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the route print to chef" do
      str =<<-EOS
script "install_something" do
  code "wget http://www.example.com/tarball.tar.gz
tar -zxf tarball.tar.gz
cd tarball
./configure
make
make install
"
  interpreter "bash"
  cwd "/tmp"
  user "root"
end
      EOS

      assert_equal str.chomp, @base.compile(@res)
    end
    
  end
  
end