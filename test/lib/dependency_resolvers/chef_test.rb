require "#{File.dirname(__FILE__)}/../../test_helper"

class ChefTest < Test::Unit::TestCase
  include PoolParty
  
  context "chef dependency_resolver test" do
    setup do
      @base = PoolParty::DependencyResolvers::Chef
      @base.compile_directory = test_dir
      
      @resources = {
        :variables => Resources::Variable.new(:animal, "Duck"),
        :files => Resources::FileResource.new(:name => "/etc/motd", :content => "Welcome to a fake file"),
        :directories => Resources::Directory.new("/etc/poolparty"),
        :http_request => PoolParty::Resources::HttpRequest.new("posting data", :url => "http://check.in", :message => {:some => "data"}, :action => :post)
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
      assert_equal "# PoolParty variables\npoolparty Mash.new unless attribute?('poolparty')\npoolparty[:animal] = \"Duck\"\n", open(test_dir/"attributes"/"poolparty.rb").read
    end
    
    should "be able to compile a file" do
      @base.compile_to(@resources[:files], test_dir)
      assert_equal "Welcome to a fake file", open(test_dir/"templates"/"default"/"etc"/"motd.erb").read
    end
    
    should "be able to compile an http_request" do
      @base.compile_to(@resources[:http_request], test_dir)
      assert_equal "http_request \"posting data\" do\n  action :post\n  url \"http://check.in\"\n  message :some => \"data\"\nend\n", open(test_dir/"recipes"/"default.rb").read
    end
    
    should "compile to the recipes" do
      @base.compile_to(@resources[:files], test_dir)
      assert_equal "template \"/etc/motd\" do\n  source \"/etc/motd.erb\"\n  action :create\n  backup 5\n  mode 0644\n  owner \"root\"\nend\n", open(test_dir/"recipes"/"default.rb").read
    end
    
    should "compile all the resources when passed the entire array" do
      resources = []
      resources << @resources[:files]
      resources << @resources[:directories]
      resources << @resources[:variables]
      @base.compile(resources)
      ["recipes"/"default.rb", "templates"/"default"/"etc"/"motd.erb"].each do |fi|
        assert File.file?(test_dir/fi)
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
  recursive false
  mode 0644
  owner "root"
  group "root"
end

EOE
      
      assert_equal output, open(test_dir/"recipes"/"default.rb").read
    end
    
  end
  
end