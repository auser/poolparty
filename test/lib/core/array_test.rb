require "test_helper"

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
      
  def test_be_able_to_select_with_hash
    assert_equal @hash_arr.select_with_hash(:name => "matt").first[:occupation], "doctor"
    assert_equal @hash_arr.select_with_hash(:occupation => "computer scientist").first[:name], "peter"
    assert @hash_arr.select_with_hash(:occupation => "matt").empty?
  end
  
end