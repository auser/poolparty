require "#{File.dirname(__FILE__)}/../../../test_helper"

include_chef_only_resources
include_fixture_resources

class VariableResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Variable.new "files", ["a_file", "b_file"]
      @base = DependencyResolvers::Chef
    end
    
    should "add the name and value as options on the variable instance" do
      assert_equal @res.name, "files"
      assert_equal @res.value, ["a_file", "b_file"]
    end
    
    should "handle printing from chef all sorts of fun variables" do
      variables = [
        PoolParty::Resources::Variable.new(:animal, "Duck"),
        PoolParty::Resources::Variable.new(:animal, {:a => "a", :b => "b"}),
        PoolParty::Resources::Variable.new(:animal, [:a, :b, :c, "d"]),
        PoolParty::Resources::Variable.new(:animal, :d),
      ]
      
      attribute_filepath = test_dir/"cookbooks"/"poolparty"/"attributes"/"poolparty.rb"
      File.unlink(attribute_filepath) if File.file?(attribute_filepath)
      p [:before, attribute_filepath]

      @base.compile_to(variables, test_dir)
      cont = open(attribute_filepath).read
      msg = <<-EOE
---
poolparty Mash.new unless attribute?("poolparty")
poolparty[:animal] = "Duck"
poolparty[:animal] = {:a => "a",:b => "b"}
poolparty[:animal] = [ :a, :b, :c, "d" ]
poolparty[:animal] = :d
      EOE
      puts cont
      puts msg
      assert_equal msg, cont
    end
    
  end
  
end