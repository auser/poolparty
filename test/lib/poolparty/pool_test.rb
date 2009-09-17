require "#{File.dirname(__FILE__)}/../../test_helper"

stub_keypair_searchable_paths

class PoolTest < Test::Unit::TestCase
  context "load_from_file" do
    setup do
      @filepath = fixtures_dir/"clouds/simple_cloud.rb"
    end

    should "load the file with load_from_file on Pool" do
      PoolParty::Pool.load_from_file(@filepath)
      assert_equal PoolParty::Pool, pools["poolparty"].class
      assert_equal PoolParty::Cloud, pools["poolparty"].clouds["simple_cloud"].class
      assert_equal "test_key", pools["poolparty"].clouds["simple_cloud"].keypair.basename
      assert_equal "/etc/motd", pools["poolparty"].clouds["simple_cloud"].files.first.name
    end
    
    should "find_and_load_default_clouds_dot_rb in Pool" do
      PoolParty::Pool.class_eval "def self.default_clouds_dot_rb_locations; [\"#{fixtures_dir/"clouds"}\"]; end"
      PoolParty::Pool.find_and_load_default_clouds_dot_rb("simple_cloud.rb")
      assert_equal PoolParty::Pool, pools["poolparty"].class
    end
  end
  
end