require "#{File.dirname(__FILE__)}/../../test_helper"

class ChefTest < Test::Unit::TestCase
  include PoolParty
  
  context "chef dependency_resolver test" do
    setup do
      @base = PoolParty::DependencyResolvers::Chef
      
      @resources = {
        :variable => Resources::Variable.new(:animal, "Duck"),
        :files => Resources::FileResource.new(:name => "/etc/motd", :content => "Welcome to a fake file")
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
      @base.compile_to(@resources[:variable], test_dir)
      assert_equal "# PoolParty variables\npoolparty Mash.new unless attribute?('poolparty')\npoolparty[:animal] = \"Duck\"\n", open(test_dir/"attributes"/"poolparty.rb").read
    end
    
    should "be able to compile a file" do
      @base.compile_to(@resources[:files], test_dir)
      assert_equal "Welcome to a fake file", open(test_dir/"templates"/"default"/"etc"/"motd.erb").read
    end
    
    should "be able to compile a file" # do
     #      p @base.compile(@resources[:file])
     #    end
  end
  
end