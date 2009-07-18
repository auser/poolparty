require "#{File.dirname(__FILE__)}/../../test_helper"

class ModTestObject
  default_attr_reader :a, ["b"]
  default_attr_reader :hsh, {}
end

class ChildModTestObject < ModTestObject
end

class ModuleTest < Test::Unit::TestCase
  
  context "default accessors" do
    setup do
      @inst = ModTestObject.new
    end

    should "set the default to an array" do
      assert_equal Array, @inst.a.class
    end
    
    should "put an object in the a variable without it being defined" do
      @inst.a << "hi"
      assert_equal ["b", "hi"], @inst.a
    end
    
    should "merge with the default hash" do
      @inst.hsh.merge!(:big => "people")
      assert_equal({:big => "people"}, @inst.hsh)
    end
    
    should "carry to the child" do
      assert_equal ["b"], ChildModTestObject.new.a
    end
  end
  
end