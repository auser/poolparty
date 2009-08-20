require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class LineResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Pool.init
      @res = PoolParty::Resources::Line.new "/etc/poolparty", :line => "hi hi"
      @base = DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the method denoted by has_method_name" do
      assert_match %r{# line in file: /etc/poolparty\n}, @res.compile(:chef)
    end
        
  end
  
end