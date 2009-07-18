require "#{File.dirname(__FILE__)}/../../test_helper"

PoolParty::Resource.define_resource_methods

class BaseTest < Test::Unit::TestCase
  context "dependency_resolver test" do
    setup do
      @base = PoolParty::DependencyResolvers::Base
    end
    should "have compile_method_name" do
      assert @base.respond_to?(:compile_method_name)
      assert_equal :print_to_base, @base.compile_method_name
    end
  end
  
  context "resolving" do
    setup do
      @base = PoolParty::DependencyResolvers::Chef
      @pool = pool :dummy do
        cloud "duh" do
          has_file "/etc/motd", :content => "piper"
        end
      end
      @cloud = @pool.clouds["duh"]
    end
    
    should "compile" do
      str =<<-EOE
template "/etc/motd" do
  content "piper"
  action :create
  backup 5
  mode 0644
  owner "root"
end
      EOE
      
      assert_equal str, @base.compile_to(@cloud.resources, test_dir)
    end
  end
  
  
end