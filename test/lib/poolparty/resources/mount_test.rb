require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class DirectoryResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Mount.new "/mnt/volume1", :device => "volume1", :device_type => :label, :fstype => "xfs", :options => "rw"
      @base = DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the mount method denoted by has_method_name" do
      str =<<-EOE
mount "/mnt/volume1" do
  action :mount
  device_type :label
  device "volume1"
  fstype "xfs"
  options "rw"
  dump 0
  pass 2
end
EOE

      assert_equal str.chomp, @base.compile(@res)
    end
    
  end
  
end