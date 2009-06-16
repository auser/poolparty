require "#{File.dirname(__FILE__)}/../../test_helper"

class BaseTestClass < PoolParty::Base
  additional_options :b, :c
  
  default_options :a => "a", :d => "dump"
end

class BaseTest < Test::Unit::TestCase
  context "Base" do
    setup do
      @inst = BaseTestClass.new
    end
    
    should "have additional_options" do
      assert_equal [:a, :b, :c, :d], @inst.dsl_options.keys.sort {|a,b| "#{a}" <=> "#{b}"}
      assert_equal "a", @inst.dsl_options[:a]
      assert_equal nil, @inst.dsl_options[:b]
      assert_equal nil, @inst.dsl_options[:c]
      assert_equal "dump", @inst.dsl_options[:d]
    end
  end
  
end