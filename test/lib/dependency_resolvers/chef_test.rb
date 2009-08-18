require "#{File.dirname(__FILE__)}/../../test_helper"

include_fixture_resources
include_chef_only_resources

class ChefTest < Test::Unit::TestCase
  include PoolParty
  
  context "chef dependency_resolver test" do
    setup do
      @base = DependencyResolvers::Chef
      @base.compile_directory = test_dir
      @cookboox_directory = test_dir/"cookbooks"/"poolparty"
      
      @resources = {
        :variables => Resources::Variable.new(:animal, "Duck"),
        :files => Resources::FileResource.new(:name => "/etc/motd", :content => "Welcome to a fake file"),
        :directories => Resources::Directory.new("/etc/poolparty"),
        :http_request => PoolParty::Resources::HttpRequest.new("posting data", :url => "http://check.in", :message => {:some => "data"}, :action => :post),
        :link => PoolParty::Resources::Link.new("/tmp/passwd", :to => "/etc/passwd"),
        :chef_recipe => PoolParty::Resources::ChefRecipe.new(fixtures_dir/"chef"/"recipes"/"sudo")
      }
    end
    
    teardown do
      # FileUtils.rm_rf test_dir
    end
    
    should "have compile to chef" do
      assert @base.respond_to?(:compile_method_name)
      assert_equal :print_to_chef, @base.compile_method_name
    end
    
    should "be able to compile a variable" do
      @base.compile_to(@resources[:variables], test_dir)
      cont = open(@cookboox_directory/"attributes"/"poolparty.rb").read
      assert_match /poolparty Mash\.new unless attribute\?\("poolparty"\)/, cont
      assert_match /poolparty\[:animal\] = \"Duck\"\n/, cont
    end
    
    should "be able to compile a file" do
      @base.compile_to(@resources[:files], test_dir)
      assert_equal "Welcome to a fake file", open(@cookboox_directory/"templates"/"default"/"etc"/"motd.erb").read
    end
    
    should "be able to compile an http_request" do
      @base.compile_to(@resources[:http_request], test_dir)
      assert_equal "http_request \"posting data\" do\n  action :post\n  url \"http://check.in\"\n  message :some => \"data\"\nend\n", open(@cookboox_directory/"recipes"/"default.rb").read
    end
    
    should "compile to the recipes" do
      @base.compile_to(@resources[:files], test_dir)
      assert_equal "template \"/etc/motd\" do\n  source \"/etc/motd.erb\"\n  action :create\n  backup 5\n  mode 0644\n  owner \"root\"\nend\n", open(@cookboox_directory/"recipes"/"default.rb").read
    end
    
    should "compile the recipes" do
      @base.compile_to(@resources[:chef_recipe], test_dir)
      assert_equal open(fixtures_dir/"chef"/"recipes"/"sudo"/"recipes"/"default.rb").read, open(@cookboox_directory/".."/"sudo"/"recipes"/"default.rb").read
      assert_equal "recipe \"sudo\"", open(@cookboox_directory/"recipes"/"default.rb").read
    end
    
    should "compile all the resources when passed the entire array" do
      resources = []
      resources << @resources[:files]
      resources << @resources[:directories]
      resources << @resources[:variables]
      @base.compile(resources)
      ["recipes"/"default.rb", "templates"/"default"/"etc"/"motd.erb"].each do |fi|
        assert File.file?(@cookboox_directory/fi)
      end
      
      output =<<-EOE
template "/etc/motd" do
  source "/etc/motd.erb"
  action :create
  backup 5
  mode 0644
  owner "root"
end

directory "/etc/poolparty" do
  action :create
  recursive true
  mode 0644
  owner "root"
  group "root"
end

EOE
      
      assert_equal output, open(@cookboox_directory/"recipes"/"default.rb").read
    end
    
    context "meta functions" do
      setup do
        PoolParty::Resource.define_resource_methods
        @inst = FakeResource.new
        @res = @resources[:link]
        @inst.has_service("apache")
      end

      should "Add meta notifies on the resource output" do        
        @res.notifies @inst.get_service("apache"), :reload
        assert_match /notifies :reload, resources\(:service => "apache"\)/, @base.compile(@res)
      end
      
      should "Add meta subscribes on the resource output" do
        @res.subscribes @inst.get_service("apache"), :reload
        assert_match /subscribes :reload, resources\(:service => "apache"\), :delayed/, @base.compile(@res)
      end
            
      should "Add meta ignore_failure on the resource output" do
        @res.ignore_failure true
        assert_match /ignore_failure true/, @base.compile(@res)
      end
      
      should "Add meta provider on the resource output" do
        @res.provider "http://google.com"
        assert_match /provider "http:\/\/google\.com"/, @base.compile(@res)
      end
      
      should "Add meta not_if on the resource output" do
        @res.not_if "test -f /etc/passwd"
        assert_match /not_if "test -f \/etc\/passwd"/, @base.compile(@res)
        @res.not_if do
File.file?("/etc/passwd")
        end
        assert_match /not_if do File.file\?\("\/etc\/passwd"\) end/, @base.compile(@res)
      end
      
      should "Add meta only_if on the resource output" do
        @res.only_if "test -f /var/poolparty/tmp"
        assert_match /only_if "test -f \/var\/poolparty\/tmp"/, @base.compile(@res)
        @res.only_if do
File.file?("/etc/passwd")
        end
        assert_match /only_if do File.file\?\("\/etc\/passwd"\) end/, @base.compile(@res)
      end
      
    end
    
  end
  
end