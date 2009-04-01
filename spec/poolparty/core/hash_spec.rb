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
  describe "method_missing" do
    it "should be able to call a key on the hash as a method" do
      {:first_name => "bob", :last_name => "frank"}.first_name.should == "bob"
    end
    it "should return nil if there is no key set in the hash" do
      {:first_name => "bob", :last_name => "frank"}.neighbor.should == nil
    end
  end
  describe "choose" do
    before(:each) do
      @selected_hash = @a.choose {|k,v| k if k == :a}
    end
    it "should return a hash when choosing" do
      @selected_hash.class.should == Hash
    end
    it "should only have the key a (choos)" do
      @selected_hash.keys.should == [:a]
    end
  end
  #TODO: deprecate
  # describe "extract!" do
  #   before(:each) do
  #     @rejected_hash = @a.extract! {|k,v| k == :a }
  #   end
  #   it "should have a reject with the keys" do
  #     @rejected_hash.keys.should == [:a]
  #   end
  #   it "should return the old array with the other keys" do
  #     @a.keys.sort.should == [:b, :c]
  #   end
  #   it "should not throw a fit with an empty hash" do
  # {}.extract!
  #       lambda {
  #       {}.extract!
  #     }.should_not raise_error
  #   end
  # end
 
  describe "append" do
    before(:each) do
      @hash = {:game => "token", :required => "for_play"}
    end
    it "should not destroy an option salready on the hash" do
      @hash.append(:required => "to_play")[:game].should == "token"
    end
    it "should change the entry to an array if it already exists" do
      @hash.append(:game => "coin")[:game].class.should == Array
    end
    it "should append the other hash's key to the end of the array" do
      @hash.append(:game => "coin")[:game][-1].should == "coin"
    end
    it "should change the hash with the append(bang) option" do
      @hash.append!(:game => "coin")
      @hash[:game][-1].should == "coin"
    end
  end
end