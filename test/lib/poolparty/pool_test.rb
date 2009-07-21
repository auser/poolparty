require "#{File.dirname(__FILE__)}/../../test_helper"

class PoolTest < Test::Unit::TestCase
  context "load_from_file" do
    setup do
      @filepath = fixtures_dir/"clouds/simple_cloud.rb"
    end

    should "load the file with load_from_file on Pool" do
      PoolParty::Pool.load_from_file(@filepath)
      assert_equal PoolParty::Pool, pools["poolparty"].class
      assert_equal PoolParty::Cloud, pools["poolparty"].clouds["app"].class
      assert_equal "test_key", pools["poolparty"].clouds["app"].keypair.basename
      assert_equal "/etc/motd", pools["poolparty"].clouds["app"].files.first.name
    end
  end
  
end