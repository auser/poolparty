require "#{File.dirname(__FILE__)}/../../test_helper"

class DefaultTest < Test::Unit::TestCase
  include PoolParty
  
  context "Default" do
    should "have default users (in the default_options)" do
      assert_equal Default.user, "root"
      assert_equal Default.poolparty_home_path, "#{ENV["HOME"]}" / ".poolparty"
      assert_equal Default.ec2_home, "#{ENV["HOME"]}" / ".ec2"
      assert_equal Default.poolparty_src_path, File.expand_path("#{File.dirname(__FILE__)}/../../../")
      assert_equal Default.base_config_directory, "/etc/poolparty"
      assert_equal Default.remote_storage_path, "/var/poolparty"
    end
  end
  
end