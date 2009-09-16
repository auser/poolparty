require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class DirectoryResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::FileResource.new "/etc/poolparty/lyrics", :content => "I'm just a file, a lonely little file in the world"
      @base = DependencyResolvers::Chef
      @base.compile_directory = test_dir
      @cookboox_directory = test_dir/"cookbooks"/"poolparty"
    end
    
    should "have the template method denoted by has_method_name" do
      str = "template \"/etc/poolparty/lyrics\" do\n  source \"/etc/poolparty/lyrics.erb\"\n  action :create\n  backup 5\n  mode \"0644\"\n  owner \"root\"\nend\n"
      
      assert_equal str, @base.compile(@res)
      assert_equal "I'm just a file, a lonely little file in the world", open(@cookboox_directory/"templates"/"default"/"etc"/"poolparty"/"lyrics.erb").read
    end
    
    should "be able to use a template instead of content" do
      @res.template(fixtures_dir/"templates"/"apache_conf.erb")
      
      @base.compile(@res)
      assert_equal "# Apache conf file\napache <%= cloud.name %>", open(@cookboox_directory/"templates"/"default"/"etc"/"poolparty"/"lyrics.erb").read
    end
    
    should "raise a TemplateNotFoundError if the template cannot be found" do
      PoolParty::PoolPartyError.create("TemplateNotFound")
      assert_raises TemplateNotFound do
        @res.template(fixtures_dir/"templates"/"non_existant_template.erb")
      end
    end
    
  end
  
end