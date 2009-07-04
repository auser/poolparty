require "#{File.dirname(__FILE__)}/../../test_helper"

class PoolTest < Test::Unit::TestCase
  context "load_from_file" do
    setup do
      @filepath = File.join(File.dirname(__FILE__), "../../../", "examples/simple.rb")
    end

    should "load the file with load_from_file on Pool" do
      PoolParty::Pool.load_from_file(@filepath)
      assert_equal pools["poolparty"].class, PoolParty::Pool
    end
  end
  
end