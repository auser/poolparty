require "#{File.dirname(__FILE__)}/../../test_helper"

class ChefTest < Test::Unit::TestCase
  include PoolParty
  
  context "chef dependency_resolver test" do
    setup do
      @base = PoolParty::DependencyResolvers::Chef
      
      @resources = {
        :variable => Resources::Variable.new(:animal, "Duck"),
        :file => Resources::FileResource.new(:name => "/etc/motd", :content => "Welcome to a fake file")
      }
    end
    
    should "have compile to chef" do
      assert @base.respond_to?(:compile_method_name)
      assert_equal :print_to_chef, @base.compile_method_name
    end
    
    should "be able to compile a variable" do
      a = @base.compile_to(@resources[:variable], test_dir)
      assert_equal "# variables\npoolparty Mash.new unless attribute?('poolparty')\npoolparty[:animal] = \"Duck\"\n", open(test_dir/"attributes"/"poolparty.rb").read
    end
    
    should "be able to compile a file" # do
     #      p @base.compile(@resources[:file])
     #    end
  end
  
end