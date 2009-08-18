require File.dirname(__FILE__) + '/test_helper.rb'

class TestTest < Test::Unit::TestCase
  def test_test_aliases
    [:test, :it, :should, :tests].each do |method_alias|
      assert self.class.respond_to?(method_alias)
    end
  end

  def test_before_test_aliases
    [:before_test, :before_it, :before_should, :before_tests].each do |method_alias|
      assert self.class.respond_to?(method_alias), method_alias.inspect
    end
  end
  
  context "A test block" do
    it "should create a test_xxx method" do
      self.class.test("should create a test method") { true }
      
      assert self.respond_to?("test: A test block should create a test method")
    end
  end
end
