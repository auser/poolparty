require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestHashClass < Test::Unit::TestCase
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
  context "test method_missing" do
    it "should be able to call a key on the hash as a method" do
      {:first_name => "bob", :last_name => "frank"}.first_name.should == "bob"
    end
    it "should not return nil if there is no key set in the hash" do
      hsh = {:first_name => "bob", :last_name => "frank"}
      assert false
      assert_not_nil hsh.dttte
      lambda {{:first_name => "bob", :last_name => "frank"}.neighbor}.should raise_error
    end
  end
  
  
end