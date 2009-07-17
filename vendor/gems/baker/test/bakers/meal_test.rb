require "#{File.dirname(__FILE__)}/../test_helper"

class MealTest < Test::Unit::TestCase
  context "template" do
    setup do
      @dir = "#{File.dirname(__FILE__)}/../test_dir"
      @meal = Baker::Meal.new(@dir)
    end
    
    should "not add a template who's file doesn't exist" do
      swallow_output do
        assert_raises StandardError do
          @meal.template "/non/existant/path"
        end        
      end
    end
    
    should "add a template where the file does exist" do
      tfile = "#{File.dirname(__FILE__)}/../fixtures/template_fixture.erb"
      @meal.template tfile
      assert_equal File.expand_path(tfile), @meal.templates.first.file
    end
    
    should "add the files in the directory" do
      tfile = "#{File.dirname(__FILE__)}/../fixtures/template_dir"
      @meal.template tfile
      assert_equal File.expand_path("#{tfile}/dumb_template.erb"), @meal.templates[0].file
      assert_equal File.expand_path("#{tfile}/smart_template.erb"), @meal.templates[2].file
      assert_equal File.expand_path("#{tfile}/inner/girls_template.erb"), @meal.templates[1].file
    end
  end
  
  context "recipe" do
    setup do
      @dir = "#{File.dirname(__FILE__)}/../test_dir"
      @meal = Baker::Meal.new(@dir)
      @recipe_path = File.expand_path("#{File.dirname(__FILE__)}/../fixtures/recipe_fixture.erb")
    end

    should "be able to create a recipe" do
      @meal.recipe @recipe_path
      assert_equal @meal.recipe_files[0], @recipe_path
    end
    
    should "have a recipe as a string" do
      @meal.recipe "hello ducks"
      @meal.compile
      assert File.file?(@dir+"/recipes/default.erb")
    end
  end
  
  context "file" do
    setup do
      @meal = Baker::Meal.new("#{File.dirname(__FILE__)}/../test_dir")
      @file_path = File.expand_path("#{File.dirname(__FILE__)}/../fixtures/files/dummy_file.pl")
    end

    should "be able to create a file" do
      @meal.files @file_path
      assert_equal @meal.files_files[0], @file_path
    end
  end
  
  context "attributes" do
    setup do
      @meal = Baker::Meal.new("#{File.dirname(__FILE__)}/../test_dir")
      @attributes_hash = {:song => "Fa Fa", :artist => "Guster"}
    end

    should "be able to create an attribute" do
      @meal.attribute @attributes_hash
      assert_equal @meal.attributes[0].attributes, @attributes_hash
    end
  end
  
  
end