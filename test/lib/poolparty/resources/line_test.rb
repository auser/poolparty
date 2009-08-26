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
    
    should "have the method denoted by has_method_name (no output by :no_print)" do
      assert_nil @res.compile(:chef)
    end
        
  end
  
end