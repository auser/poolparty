require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class ServiceResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Service.new "example_service", 
                                                :action => [:enable, :start],
                                                :supports => {:status => true, :restart => true, :reload => true}
      @base = PoolParty::DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the mount method denoted by has_method_name" do

str = <<-EOS
service "example_service" do
  pattern "example_service"
  action :[ :enable, :start ]
  supports :reload => true,
:status => true,
:restart => true
end
EOS

      assert_equal str.chomp, @base.compile(@res)
    end
    
  end
  
end