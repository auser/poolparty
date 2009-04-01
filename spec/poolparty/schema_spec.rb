require "rubygems"
require "spec"
require "json"
require File.dirname(__FILE__) + '/../../lib/poolparty/schema'

describe "Schema" do
  it "should not fail when called with a hash" do
    lambda {PoolParty::Schema.new({:a => "a"})}.should_not raise_error
  end
  it "should be able to take a string of JSON" do
    lambda{PoolParty::Schema.new({:a => "a"}.to_json)}.should_not raise_error
  end
  describe "methods" do
    before(:each) do
      @schema = PoolParty::Schema.new :a => "b", :b => {:a => "a in b", :b => {:a => "a in b.b"}}
    end
    it "should be able to call a method that's in the hash on the schema" do
      @schema.a.should == "b"
    end
    it "should be able to call deeply into the hash" do
      @schema.b.a.should == "a in b"
    end
    it "should be able to call really deep into the hash" do
      @schema.b.b.a.should == "a in b.b"
    end
  end
  describe "json" do
    before(:each) do
      @schema = PoolParty::Schema.new({:a => "b", :b => {:a => "a in b", :b => {:a => "a in b.b"}}}.to_json)
    end
    it "should be able to call a method that's in the hash on the schema" do
      @schema.a.should == "b"
    end
    it "should be able to call deeply into the hash" do
      @schema.b.a.should == "a in b"
    end
    it "should be able to call really deep into the hash" do
      @schema.b.b.a.should == "a in b.b"
    end
  end
  describe "with arrays" do
    before(:each) do
      @schema = PoolParty::Schema.new({:a => ["b"], :b => {:a => ["a in b", "a in b.a"], :b => {:a => "a in b.b"}}}.to_json)
    end
    it "should have an array in a" do
      @schema.a.class.should == Array
      @schema.a.should == ["b"]
    end
    it "should have an array in b (deep)" do
      @schema.b.a.should == ["a in b", "a in b.a"]
    end
  end
end