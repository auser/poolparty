require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestHash < Test::Unit::TestCase
  context "hash_get" do
    before do
      @hsh = {
        "10.0.0.3" => {"stuff" => "here"},
        "10.0.0.1" => {"stuff" => "here"},
        "10.0.0.2" => {"stuff" => "here"}
      }
    end
    it "should return 0.1 if we are 0.2" do
      assert @hsh.next_sorted_key("10.0.0.1"), "10.0.0.2"
      assert @hsh.next_sorted_key("10.0.0.2"), "10.0.0.3"
      assert @hsh.next_sorted_key("10.0.0.3"), "10.0.0.1"
      assert @hsh.next_sorted_key("10.0.0.1"), "10.0.0.2"
    end
    it "should return self if there is only 1 element" do
      k = {"10.0.0.2" => {"stuff" => "here"}}.next_sorted_key("10.0.0.2")
      assert k, "10.0.0.2"
    end
  end
end