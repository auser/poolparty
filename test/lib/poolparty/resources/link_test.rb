require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class LinkResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Link.new "/tmp/passwd", :to => "/etc/passwd"
      @base = DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the link method denoted by has_method_name" do
      str = 'link "/tmp/passwd" do
  link_type :symbolic
  action :create
  to "/etc/passwd"
end
'

      assert_equal str, @base.compile(@res)
    end
    
  end
  
end