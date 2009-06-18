require "#{File.dirname(__FILE__)}/../../test_helper"

class StringTest < Test::Unit::TestCase
  context "string" do
    
    should "replace with ^" do
      out = ":god bless :country" ^ {:god => "Budda", :country => "India"}
      assert_equal "Budda bless India", out
    end
    
  end
  
end