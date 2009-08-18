require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class ConditionalResourceTest < Test::Unit::TestCase
    
  def test_have_the_cron_method_denoted_by_has_method_name
    PoolParty::Resource.define_resource_methods
    @res = PoolParty::Resources::Conditional.new "os" do
      when_is :ubuntu, "git-core"
      else_is "git"
    end
    @base = DependencyResolvers::Chef
    @base.compile_directory = test_dir
    
    str = 'case "os"
when :ubuntu
  "git-core"
else
  "git"
end'

    assert_equal str, @base.compile(@res)
  end
  
  def test_compile_from_cloud
    pool "conditional_cloud" do
      cloud "test" do
        
        has_case "os" do
          when_is :ubuntu, "git-core"
          else_is "git"
        end
        
      end
    end
    
    str = 'case "os"
when :ubuntu
  "git-core"
else
  "git"
end'

    assert_equal str, clouds["test"].compile.chomp
    
  end
  
end