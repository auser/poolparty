require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestNeighborhoods < Test::Unit::TestCase
  context "Constant" do
    should "have the constant Neighborhoods" do
      PoolParty.const_get("Neighborhoods").should == PoolParty::Neighborhoods
    end
  end
  context "setting up a neighborhood" do
    should "set the schema when starting with a json string or a hash" do
      n = Neighborhoods.new({:instances => "b"})
      n.schema.nil?.should == false
      n.schema.class.should == PoolParty::Schema
    end
    should "raise an error if there started with nil" do
      lambda {Neighborhoods.new}.should raise_error
    end
    should "should not raise a fit if there ARE instances defined" do
      lambda{Neighborhoods.new({:instances => ["10.0.0.1"]})}.should_not raise_error
    end
  end
  
end