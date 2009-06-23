require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestWrappingArray < Test::Unit::TestCase
  context "wrapping_next" do
    setup do
      @array = %w(a b c d)
    end

    should "return element next in the chain" do
      @array.wrapping_next("a").should == "b"
      @array.wrapping_next("b").should == "c"
      @array.wrapping_next("c").should == "d"
      @array.wrapping_next("d").should == "a"
      @array.wrapping_next("a").should == "b"
    end
    should "raise if the element is not in the array" do
      lambda {@array.wrapping_next("z")}.should raise_error
    end
  end
  
  context "histogram" do
    should "provide a histogram" do
      out = %w(tart q a b a a b c).histogram
      assert_equal 1, out["tart"]
      assert_equal 3, out["a"]
      assert_equal 2, out["b"]
      assert_equal 1, out["c"]
    end
  end
  
  
end