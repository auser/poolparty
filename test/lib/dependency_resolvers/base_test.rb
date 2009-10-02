require "#{File.dirname(__FILE__)}/../../test_helper"

PoolParty::Resource.define_resource_methods

class BaseTest < Test::Unit::TestCase
  
  def test_have_compile_method_name
    assert DependencyResolvers::Base.respond_to?(:compile_method_name)
    assert_equal :print_to_base, DependencyResolvers::Base.compile_method_name
  end
  def test_have_a_list_of_all_the_DependencyResolvers
    assert DependencyResolvers.all.include?(DependencyResolvers::Chef)
  end
  
  
  def setup
    clear!
    @base = DependencyResolvers::Chef
    @pool = pool "dummy test cloud" do
      cloud "duh" do
        keypair "test_key", fixtures_dir/"keys"
        file "/etc/motd", :content => "piper"
      end
    end
    @cloud = @pool.clouds["duh"]
  end
  
  def test_compile_base
      str =<<-EOE
template "/etc/motd" do
  source "/etc/motd.erb"
  action :create
  backup 5
  mode "0644"
  owner "root"
end
      EOE
      
    assert_equal str, @base.compile_to(@cloud.resources, test_dir)
  end
  
  
end