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
  
  def test_swap
    assert_equal %w(a c b d), @arr.swap!(1,2)
    assert_equal %w(d c b a), @arr.swap!(0,3)
    assert_equal %w(d c a b), @arr.swap!(2,3)
  end
  
  def test_zip_offset
    arr = %w(a b c d e f)
    assert_equal [["a","b"],["b","c"],["c","d"],["d","e"],["e","f"]], arr.zip_offset(1)
    assert_equal [["a","b"],["b","c"],["c","d"],["d","e"]], arr.zip_offset(2)
    assert_equal [["a","b"]], arr.zip_offset(5)
  end
  
  def test_rotate
    arr = %w(a b c d e)
    assert_equal %w(b c d e a), arr.rotate
  end
  
end