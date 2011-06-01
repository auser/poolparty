require 'test_helper'

class HashTest < Test::Unit::TestCase
  context "Hash" do
    setup do
      @hsh = {:a => "a", :b => "b", :c => "c"}
    end
    
    should "have choose to select hashes" do      
      assert_equal @hsh.choose {|k,v| k == :a}, {:a => "a"}
      assert_equal @hsh.choose {|k,v| k == :z}, {}
      assert_equal @hsh.choose {|k,v|[:b,:a].include?(k)}.keys.sort{|a,b|"#{a}"<=>"#{b}"}, [:a,:b]
    end
            
    should "be able to call methods on the hash of their keys" do
      assert_equal @hsh.a, "a"
      assert_equal @hsh.b, "b"
      assert_equal @hsh.c, "c"
    end
    
    should "compute the differences in the hashes" do
      assert_equal ({:a => "a"}).diff({:a => "a"}), {}
      assert_equal ({:a => "a"}).diff({:a => "b"}), {:a => "b"}
      assert_equal ({:a => "a", :b => "b"}).diff({:a => "b"}, :a), {:a => "b"}
      assert_equal ({:a => "a", :b => "b"}).diff({:b => "b"}, :b), {}
      assert_equal ({:a => "a", :b => "b"}).diff({:b => "c"}, :b), {:b => "c"}
      assert_equal ({:a => "a", :b => "b"}).diff({:b => "c"}), {:b => "c", :a => nil}
      assert_equal ({:a => "a", :b => "b"}).diff({:b => "c"}, :a), {:a => nil}
    end
    
  end
  
end