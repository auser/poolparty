require File.dirname(__FILE__) + '/../spec_helper'

describe "OrderedHash" do
  before(:each) do
    @oh = OrderedHash.new
    @oh["var1"]=123
    @oh["var2"]=2
    @oh["var3"]=3
    @oh["var4"]=23
  end
  it "should stay in order" do
    @oh.collect { |k,v| k }.should  == ["var1", "var2", "var3", "var4"]
  end
  it "should not raise an error when called with any Hash method" do
    pending
  end
  
end
