require "#{File.dirname(__FILE__)}/../../test_helper"

class ArrayTest < Test::Unit::TestCase
  context "Array" do
    setup do
      @arr = %w(a b c d)
      @hash_arr = [
                    {:name => "peter", :occupation => "computer scientist"},
                    {:name => "al", :occupation => "computer scientist"},
                    {:name => "matt", :occupation => "doctor"},
                    {:name => "jenna", :occupation => "lawyer"}
                  ]
    end
    should "have collect_with_index" do
      out = []
      @arr.collect_with_index do |ele, i|
        out << "#{ele}#{i+1}"
      end
      assert_equal ["a1", "b2", "c3", "d4"], out
    end
    
    should "be able to select_with_hash" do
      assert_equal @hash_arr.select_with_hash(:name => "matt").first[:occupation], "doctor"
      assert_equal @hash_arr.select_with_hash(:occupation => "computer scientist").first[:name], "peter"
      assert @hash_arr.select_with_hash(:occupation => "matt").empty?
    end
    
    should "be able to wrap with the next" do
      assert_equal @arr.wrapping_next("a"), "b"
      assert_equal @arr.wrapping_next("b"), "c"
      assert_equal @arr.wrapping_next("c"), "d"
      assert_equal @arr.wrapping_next("d"), "a"
    end
  end
  
end