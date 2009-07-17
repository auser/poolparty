require "#{File.dirname(__FILE__)}/../test_helper"

class RecipeTest < Test::Unit::TestCase
  
  should "open the recipe content when called with an existing recipe file" do
    assert_equal "package \"dummy_package\" do\n  action :install\nend", Baker::Recipe.new("#{File.dirname(__FILE__)}/../fixtures/recipe_fixture.erb").content
  end
  should "have a cookbook_directory" do
    recipe = Baker::Recipe.new( :file => "#{File.dirname(__FILE__)}/../fixtures/recipe_fixture.erb", 
                                    :cookbook_directory => "#{File.dirname(__FILE__)}/../test_dir")
    assert_equal "#{File.dirname(__FILE__)}/../test_dir", recipe.cookbook_directory
  end
  context "compiling" do
    
    setup do
      @cookbook_directory = "#{File.dirname(__FILE__)}/../test_dir"
      @recipe_file = "#{File.dirname(__FILE__)}/../fixtures/recipe_fixture.erb"
      @recipe = Baker::Recipe.new( :file => @recipe_file, :cookbook_directory => @cookbook_directory, :template_name => "burbary")
      FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
    end
    
    teardown do
      FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
    end
    
    should "accept a string as a recipe" do
      assert !File.directory?("#{@cookbook_directory}/recipes")
      Baker::Recipe.new( :string => "file :name do\nmode 0755\nend", :cookbook_directory => @cookbook_directory, :template_name => "hi").compile
      assert File.directory?("#{@cookbook_directory}/recipes")
      assert File.file?("#{@cookbook_directory}/recipes/hi.erb")
      assert_equal "file :name do\nmode 0755\nend\n", open("#{@cookbook_directory}/recipes/hi.erb").read
      Baker::Recipe.new( :string => "directory '/etc/poolparty' do\nend", :cookbook_directory => @cookbook_directory, :template_name => "hi").compile
      assert_equal "file :name do\nmode 0755\nend\ndirectory '/etc/poolparty' do\nend\n", open("#{@cookbook_directory}/recipes/hi.erb").read
    end
    
    should "create the recipe directory (since it doesn't exist)" do
      assert !File.directory?("#{@cookbook_directory}/recipes")
      @recipe.compile
      assert File.directory?("#{@cookbook_directory}/recipes")
    end
    should "store the content in the new file" do
      @recipe.compile
      assert_equal "package \"dummy_package\" do\n  action :install\nend", open("#{@cookbook_directory}/recipes/burbary.erb").read
    end
  end
  
end
