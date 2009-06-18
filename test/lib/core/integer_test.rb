require "#{File.dirname(__FILE__)}/../../test_helper"

class IntegerTest < Test::Unit::TestCase
  context "Integer" do
    should "have months defined" do
      # 60 seconds * 60 minutes * 24 hours * 30 days
      assert_equal 60*60*24*30, 1.months
      assert_equal 60*60*24*30*2, 2.months
      assert_equal 60*60*24*30*12, 12.months
    end
    
    should "have years set" do
      # 60 seconds * 60 minutes * 24 hours * 365.25 days
      assert_equal 60*60*24*365.25, 1.year
      assert_equal 60*60*24*365.25*2, 2.years
    end
  end
  
end