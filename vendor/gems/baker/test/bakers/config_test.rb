require "#{File.dirname(__FILE__)}/../test_helper"

class ConfigTest < Test::Unit::TestCase
  context "config" do
    should "accept a file and that file will be placed in the file var" do
      c = Baker::Config.new("#{File.dirname(__FILE__)}/../fixtures/config_fixture.erb")
      assert_equal open("#{File.dirname(__FILE__)}/../fixtures/config_fixture.erb").read, c.content
    end
    should "accept a string that will turn into the content" do
      c = Baker::Config.new("config for chef")
      assert_equal "config for chef", c.content
    end
    should "use the default config if nothing is passed" do
      c = Baker::Config.new
      assert_equal "cookbook_path     \"/etc/chef/cookbooks\"\nnode_path         \"/etc/chef/nodes\"\nlog_level         :info\nfile_store_path  \"/etc/chef\"\nfile_cache_path  \"/etc/chef\"\n", c.content
    end
    
    context "compiling" do
      setup do
        @cookbook_directory = "#{File.dirname(__FILE__)}/../test_dir"
        @config = Baker::Config.new("config")
        @config.cookbook_directory = @cookbook_directory
        FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
      end

      teardown do
        FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
      end

      should "create the template directory (since it doesn't exist)" do
        assert !File.directory?("#{@cookbook_directory}/config")
        @config.compile("burbary")
        assert File.directory?("#{@cookbook_directory}/config")
      end
      should "store the content in the new file" do
        @config.compile("burbary")
        assert_equal "config", open("#{@cookbook_directory}/config/burbary").read
      end
    end
  end
  
end
