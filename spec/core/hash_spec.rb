require File.dirname(__FILE__) + '/../spec_helper'

describe "Hash" do
  before(:each) do
    @a = {:a => "10", :b => "20", :c => "30"}
  end
  it "should preserve the contents of the original hash when safe_merge'ing" do
    a = {:a => "10", :b => "20"}
    b = {:b => "30", :c => "40"}
    a.safe_merge(b).should == {:a => "10", :b => "20", :c => "40"}
  end
  it "should preserve the contents of the original hash when safe_merge!'ing" do
    a = {:a => "10", :b => "20"}
    b = {:b => "30", :c => "40"}
    a.safe_merge!(b)
    a.should == {:a => "10", :b => "20", :c => "40"}
  end
  it "should be able to turn itself into an open struct" do
    @a.to_os.class.should == MyOpenStruct
  end
  it "should respond to to_hash" do
    @a.to_os.respond_to?(:to_hash).should == true
  end
  it "should be able to turn itself into an open struct with the method to_hash on the object" do
    @a.to_os.to_hash.should == @a
  end
  it "should be able to flush out into a string into an array" do
    @a.flush_out.should == ["a => '10'","b => '20'","c => '30'"]
  end
  it "should be able to flush out with prev and posts" do
    @a.flush_out("hi", "ho").should == ["hia => '10'ho","hib => '20'ho","hic => '30'ho"]
  end
end