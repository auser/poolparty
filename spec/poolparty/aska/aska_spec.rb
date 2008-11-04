require File.dirname(__FILE__) + '/../spec_helper'

class Car
  include Aska
  rules :names, [
      "x > 0",
      "y > 0",
      "x > y"
    ]
end
describe "Rules" do
  before(:each) do
    @car = Car.new
  end
  describe "malformed rules" do
    it "should not put a malformed rule in the rules" do
      lambda {
        @plaster = Class.new do
          include Aska
          rules :names, "x runs b"
        end
      }.should raise_error()
    end
    [
      "k > 10",
      "x == 2",
      "x >= 2",
      "x <= 4",
      "y < 10"
    ].each do |str|
      eval <<-EOE
        it "should have the rule #{str} be valid" do
          (str =~ /(.+)[=\\\<\>](.*)/).nil?.should == false
        end
      EOE
    end
    [
      "k running 10",
      "10 is larger than 2"
    ].each do |str|
      eval <<-EOE
        it "should have the rule #{str} not be valid" do
          (str =~ /(.+)[=\\\<\>](.*)/).nil?.should == true
        end
      EOE
    end
  end
  it "should be able to define rules as an array and they should be set as the rules on the class" do
    @car.rules.class.should == Hash
  end
  it "should be able to look up the rules based on the name into an array" do
    @car.names.class.should == Aska::Rules
  end
  it "should be able to turn them into a string" do
    @car.names.to_s.should == "'x > 0', 'y > 0', 'x > y'"
  end
  it "should be able to say that rules are defined when they are defined" do
    @car.names.should_not be_nil
  end
  it "should be able tos ay that rules are not defined when they are not defined" do
    @car.look_up_rules(:cars_and_wheels).should be_empty
  end
  it "should be able to say if the rules are not rules" do
    @car.are_rules?(:cars_and_wheels).should be_false
  end
  it "should be able to say that rules are rules" do
    @car.are_rules?(:names).should be_true
  end
  describe "parsing" do
    it "should be able to parse the x > 0 into an array" do
      @car.names.include?({"x"=>[">","0"]}).should == true
    end
    it "should be able to parse y > 0 into an array" do
      @car.names.include?({"y"=>[">","0"]}).should == true
    end
    it "should be able to parse x > y into the hash" do
      @car.names.include?({"x"=>[">","y"]}).should == true
    end
    it "should have 3 rules" do
      @car.names.size.should == 3
    end
    it "should be able to look up the names as a rule" do
      Car.look_up_rules(:names).should == [{"x"=>[">", "0"]}, {"y"=>[">", "0"]}, {"x"=>[">", "y"]}]
    end
  end
  # it "should use x if available instead of x_aska" do
  #   @car.x = 6
  #   @car.get_var(:x).class.should == @car.x_aska.class
  # end
  # it "should be able to get the variable associated with the instance and return it" do    
  #   @car.x_aska = 4
  #   @car.get_var(:x).class.should == @car.x_aska.class
  # end
  it "should be able to get a number with the instance and return it as a float" do
    @car.__aska_get_var(4).class.should == Float
  end
  it "should be able to get the method it's applying as a method symbol" do
    @car.__aska_get_var(:<).class.should == Symbol
  end
  it "should be able to get the method as a symbol" do
    @car.__aska_get_var("<").class.should == Symbol
  end
  it "should be able to retrieve the value of the rule when checking if it's valid" do
    @car.x = 10
    @car.valid_rule?({:x => [:==, 10]}).should == true
  end
  it "should be able to apply the rules and say that they are not met when they aren't" do
    @car.valid_rules?(:names).should == false
  end
  it "should be able to apply the rules and say they aren't valid when they aren't all met" do
    @car.x = 5
    @car.y = 10
    @car.valid_rules?(:names).should == true
  end
  it "should be able to apply the rules and say they aren't valid when they aren't all met" do
    @car.x = 5
    @car.y = 0
    @car.valid_rules?(:names).should == false
  end
  it "should be able to apply the rules and say that they are in fact valid" do
    @car.x = 10
    @car.y = 5    
    @car.valid_rules?(:names).should == true
  end
  it "should have the rules in an array of hashes" do
    @car.names.each do |n|
      
    end
  end
end