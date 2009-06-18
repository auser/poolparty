require "#{File.dirname(__FILE__)}/../../test_helper"

class SymbolTest < Test::Unit::TestCase
  context "Symbol" do
    should "be able to compare to strings" do
      assert_equal [:a, :c, :b].sort, [:a, :b, :c]
      assert_nothing_raised do
        :a <=> :b
      end
    end
    
    should "have path separaters" do
      assert_equal :a / :b, "a/b"
    end
    
    should "be able to be sanitized" do
      assert_equal "abcd", "a.b-c d".to_sym.sanitize
    end
    
  end
  
end