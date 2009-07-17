require "#{File.dirname(__FILE__)}/../test_helper"

class TemplateTest < Test::Unit::TestCase
  should "raise if the file passed is not a file" do
    assert_raise StandardError do
      Baker::Template.new("not a file")
    end
  end
  should "open the template content when called with an existing template file" do
    assert_equal "Just a template called <%= name %>\n", Baker::Template.new("#{File.dirname(__FILE__)}/../fixtures/template_fixture.erb").content
  end
  should "have a cookbook_directory" do
    template = Baker::Template.new( :file => "#{File.dirname(__FILE__)}/../fixtures/template_fixture.erb", 
                                    :cookbook_directory => "#{File.dirname(__FILE__)}/../test_dir")
    assert_equal "#{File.dirname(__FILE__)}/../test_dir", template.cookbook_directory
  end
  context "compiling" do
    setup do
      @cookbook_directory = "#{File.dirname(__FILE__)}/../test_dir"
      @template_file = "#{File.dirname(__FILE__)}/../fixtures/template_fixture.erb"
      @template = Baker::Template.new( :file => @template_file, :cookbook_directory => @cookbook_directory)
      FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
    end
    
    teardown do
      FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
    end

    should "create the template directory (since it doesn't exist)" do
      assert !File.directory?("#{@cookbook_directory}/templates/default")
      @template.compile
      assert File.directory?("#{@cookbook_directory}/templates/default")
    end
    should "store the content in the new file" do
      @template.compile
      assert_equal "Just a template called <%= name %>\n", open("#{@cookbook_directory}/templates/default/default.erb").read
    end
    should "have the all method to reveal all the templates" do
      assert_equal [], @template.all
      @template.compile
      assert_equal ["default.erb"], @template.all
    end
  end
  
end
