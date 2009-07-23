require "#{File.dirname(__FILE__)}/../../test_helper"

class ArrayTest < Test::Unit::TestCase
  def setup
    @arr = %w(a b c d)
    @hash_arr = [
                  {:name => "peter", :occupation => "computer scientist"},
                  {:name => "al", :occupation => "computer scientist"},
                  {:name => "matt", :occupation => "doctor"},
                  {:name => "jenna", :occupation => "lawyer"}
                ]
  end
    
  def test_have_collect_with_index
    out = []
    @arr.collect_with_index do |ele, i|
      out << "#{ele}#{i+1}"
    end
    assert_equal ["a1", "b2", "c3", "d4"], out
  end
    
  def test_be_able_to_select_with_hash
    assert_equal @hash_arr.select_with_hash(:name => "matt").first[:occupation], "doctor"
    assert_equal @hash_arr.select_with_hash(:occupation => "computer scientist").first[:name], "peter"
    assert @hash_arr.select_with_hash(:occupation => "matt").empty?
  end
    
  def test_should_be_able_to_wrap_with_the_next
    assert_equal @arr.wrapping_next("a"), "b"
    assert_equal @arr.wrapping_next("b"), "c"
    assert_equal @arr.wrapping_next("c"), "d"
    assert_equal @arr.wrapping_next("d"), "a"
  end
  
end