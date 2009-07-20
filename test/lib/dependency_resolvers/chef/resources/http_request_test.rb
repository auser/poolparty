require "#{File.dirname(__FILE__)}/../../../../test_helper"

include_fixture_resources
include_chef_only_resources

class HttpResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::HttpRequest.new "posting data", :url => "http://example.com/check_in", :message => {:some => "data"}, :action => :post
      @base = PoolParty::DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the method denoted by has_method_name" do
      str =<<-EOE
http_request "posting data" do
  action :post
  url "http://example.com/check_in"
  message :some => "data"
end
      EOE

      assert_equal str, @base.compile(@res)
    end
    
  end
  
end