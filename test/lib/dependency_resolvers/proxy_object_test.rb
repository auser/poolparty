require "#{File.dirname(__FILE__)}/../../test_helper"

include_fixture_resources

PoolParty::Resource.define_resource_methods

class FakeObject < PoolParty::Resource
  
  default_options(
    :name => "Faker"
  )
  
  def initialize(hsh)
    set_vars_from_options(hsh)
  end
  
  def default_value
    "default, fo shiz"
  end
  
  def print_to_chef
    <<-EOE
fake :<%= name %> do
  name "<%= name %>"
  chap "<%= chap ? true : false %>"
  default_value "<%= default_value %>"
end
<%= print_resources %>
EOE
  end
end

class ProxyObjectTest < Test::Unit::TestCase
  include DependencyResolvers
  
  context "ProxyObject" do
    
    setup do
      @faker = FakeObject.new(:cap => "keyyyyy", :chap => "man")
      @faker.has_tester "/etc/motd", :content => "box"
      @po = ProxyObject.new(@faker)
    end
    
    should "default to the method_missing of the object" do      
      assert_equal "keyyyyy", @po.cap
      assert_equal "man", @po.chap
      assert_raises NoMethodError do
        @po.snakes
      end
    end
    
    should "compile with chef" do
      str = @po.compile(:print_to_chef)
      output = 'fake :Faker do
  name "Faker"
  chap "true"
  default_value "default, fo shiz"
end
fake "/etc/motd" do
  content "box"
end
'
      assert_equal str.chomp, output
    end
    
    should "have default options printed in layout with print_dsl_options" do
      str = @po.print_dsl_options(":key: ':value'")
      assert_match /name: 'Faker'/, str
      assert_match /cap: 'keyyyyy'/, str
      assert_match /chap: 'man'/, str
    end
    
  end
  
end