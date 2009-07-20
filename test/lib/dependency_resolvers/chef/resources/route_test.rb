require "#{File.dirname(__FILE__)}/../../../../test_helper"

include_fixture_resources
include_chef_only_resources

class RouteResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Route.new "20.0.0.0" do
        gateway "10.0.0.20"
        metric 5
        route_type :net
        netmask "255.255.0.0"
      end
      @res.does_not_exist!
      @base = PoolParty::DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the route print to chef" do
      str =<<-EOE
route "20.0.0.0" do
  gateway "10.0.0.20"
  metric 5
  route_type :net
  netmask "255.255.0.0"
  action :delete
end
EOE

      assert_equal str.chomp, @base.compile(@res)
    end
    
  end
  
end