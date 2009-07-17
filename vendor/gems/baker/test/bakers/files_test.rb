require "#{File.dirname(__FILE__)}/../test_helper"

class RecipeTest < Test::Unit::TestCase
  should "raise if the file to Files passed is not a file" do
    assert_raise StandardError do
      Baker::Files.new("not a file")
    end
  end
  should "have a files_directory" do
    recipe = Baker::Files.new( :file => "#{File.dirname(__FILE__)}/../fixtures/recipe_fixture.erb", 
                                    :cookbook_directory => "#{File.dirname(__FILE__)}/../test_dir")
    assert_equal "#{File.dirname(__FILE__)}/../test_dir", recipe.cookbook_directory
  end
  context "compiling" do
    setup do
      @cookbook_directory = "#{File.dirname(__FILE__)}/../test_dir"
      @files_file = "#{File.dirname(__FILE__)}/../fixtures/recipe_fixture.erb"
      @recipe = Baker::Files.new( :file => @files_file, :cookbook_directory => @cookbook_directory)
      FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
    end
    
    teardown do
      FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
    end

    should "create the files directory (since it doesn't exist)" do
      assert !File.directory?("#{@cookbook_directory}/files")
      @recipe.compile
      assert File.directory?("#{@cookbook_directory}/files")
    end
    should "store the file" do
      @recipe.compile
      assert_equal "package \"dummy_package\" do\n  action :install\nend", open("#{@cookbook_directory}/files/default.erb").read
    end
  end
  
end
