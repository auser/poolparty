require "#{File.dirname(__FILE__)}/../../../test_helper"

include_chef_only_resources
include_fixture_resources

class VariableResourceTest < Test::Unit::TestCase

  def setup
    PoolParty::Resource.define_resource_methods
    @res = PoolParty::Resources::Variable.new "files", ["a_file", "b_file"]
    @base = DependencyResolvers::Chef
    
    FileUtils.rm_rf test_dir
    
    @comp_dir = test_dir/"tests"/"something"/"new"
    @attribute_filepath = @comp_dir/"cookbooks"/"poolparty"/"attributes"/"poolparty.rb"    
  end
  
  def test_add_the_name_and_value_as_options_on_the_variable_instance
    assert_equal @res.name, "files"
    assert_equal @res.value, ["a_file", "b_file"]
  end
  
  def test_handle_printing_from_chef_all_sorts_of_fun_variables
    vars = [
      PoolParty::Resources::Variable.new(:the_animal, "Duck"),
      PoolParty::Resources::Variable.new(:hash, {:b => "b"}),
      PoolParty::Resources::Variable.new(:array, [:a, :b, :c, "d"]),
      PoolParty::Resources::Variable.new(:symbol, :d),
    ]

    FileUtils.rm(@attribute_filepath) if File.file?(@attribute_filepath)
    
    @base.compile_to(vars, @comp_dir)
    cont = open(@attribute_filepath).read

    assert_match /poolparty\[:the_animal\] = "Duck"/, cont
    assert_match /poolparty\[:hash\] = \{:b => "b"\}/, cont
    assert_match /poolparty\[:array\] = \[ :a, :b, :c, "d" \]/, cont
    assert_match /poolparty\[:symbol\] = :d/, cont
  end

end