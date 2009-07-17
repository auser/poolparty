require "#{File.dirname(__FILE__)}/../test_helper"

class IncludeCookbookTest < Test::Unit::TestCase
  context "include cookbook" do
    
    should "raise an error if the directory given does not exist" do
      assert_raise StandardError do
        Baker::IncludeCookbook.new("not a directory")
      end
    end
    
    should "not raise an error if the directory does exist" do
      assert_nothing_raised StandardError do
        Baker::IncludeCookbook.new("#{File.dirname(__FILE__)}/../fixtures/cookbook_directory")
      end
    end
    
    should "have the cookbook name (the name of the directory)" do
      assert_equal Baker::IncludeCookbook.new("#{File.dirname(__FILE__)}/../fixtures/cookbook_directory").cookbook_name, "cookbook_directory"
    end
    
    context "compiling" do
      setup do
        @cookbook_directory = "#{File.dirname(__FILE__)}/../test_dir"
        @cookbook_path = "#{File.dirname(__FILE__)}/../fixtures/cookbook_directory"
        @cookbook = Baker::IncludeCookbook.new( :directory => @cookbook_path, :cookbook_directory => @cookbook_directory)
        FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
      end

      teardown do
        # FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
      end
      
      should "create the template directory (since it doesn't exist)" do
        assert !File.directory?("#{@cookbook_directory}/cookbooks")
        @cookbook.compile
        assert File.directory?("#{@cookbook_directory}/cookbooks/cookbook_directory")
      end
      
      should "copy recursively the cookbook files" do
        @cookbook.compile
        assert File.file?("#{@cookbook_directory}/cookbooks/cookbook_directory/recipes/default.rb")
        assert File.file?("#{@cookbook_directory}/cookbooks/cookbook_directory/templates/cookbook_template.erb")
      end
      
    end
    
    
  end
  
end