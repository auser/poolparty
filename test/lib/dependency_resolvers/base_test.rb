require "#{File.dirname(__FILE__)}/../../test_helper"

class BaseTest < Test::Unit::TestCase
  context "dependency_resolver test" do
    setup do
      @base = PoolParty::DependencyResolvers::Base.new
    end
    should "have an id" do
      assert @base.respond_to?(:name)
    end
  end
  
end