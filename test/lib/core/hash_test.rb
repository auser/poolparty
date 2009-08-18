require "#{File.dirname(__FILE__)}/../../test_helper"

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
    
    should "set the hash as variables on the instance" do
      @inst = Object.new
      @hsh.to_instance_variables(@inst)
      assert_equal @inst.a, "a"
      assert_equal @inst.b, "b"
      assert_equal @inst.c, "c"
    end
    
    should "be able to retrieve values_at" do
      assert_equal @hsh.values_at(:a, :b), ["a", "b"]
      assert_equal @hsh.values_at(:z), []
    end
    
    should "symbolize_keys and stringify_keys" do
      hsh = {"a" => "a", "b" => "b", "c" => "c"}
      assert_equal @hsh, hsh.symbolize_keys
      assert_equal hsh, @hsh.stringify_keys
    end
    
    should "be able to call methods on the hash of their keys" do
      assert_equal @hsh.a, "a"
      assert_equal @hsh.b, "b"
      assert_equal @hsh.c, "c"
    end
  end
  
end