require File.dirname(__FILE__) + '/../spec_helper'

describe "Configuer" do
  before(:each) do
    @conf = Object.new
  end
  it "should not be nil" do
    @conf.should_not be_nil
  end
  
end