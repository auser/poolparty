require "#{File.dirname(__FILE__)}/../../test_helper"

class PoolTest < Test::Unit::TestCase
  context "load_from_file" do
    setup do
      @filepath = File.join(File.dirname(__FILE__), "../../../", "examples/simple.rb")
    end

    should "load the file with load_from_file on Pool" do
      PoolParty::Pool.load_from_file(@filepath)
      assert_equal PoolParty::Pool, pools["poolparty"].class
      assert_equal PoolParty::Cloud, pools["poolparty"].clouds["app"].class
      assert_equal "cloudteam_test", pools["poolparty"].clouds["app"].keypair
      assert_equal "/etc/motd", pools["poolparty"].clouds["app"].files.first.name
    end
  end
  
end