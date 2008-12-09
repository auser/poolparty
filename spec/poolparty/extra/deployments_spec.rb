require File.dirname(__FILE__) + '/../spec_helper'

describe "deployments" do
  describe "class" do
    before(:each) do
      @klass = PoolParty::Extra::Deployments
      @file = "hanky_danky_ranky.pool"
      ::File.stub!(:file?).with(@file).and_return false
    end
    it "should have the singleton method include_deployment" do
      @klass.respond_to?(:include_deployment).should == true
    end
    it "should return nil if the file doesn't exist" do
      @klass.include_deployment(@file).should == nil
    end
    describe "existing file" do
      before(:each) do
        ::File.stub!(:file?).and_return true
        @contents = "'hello goober'"
        @klass.stub!(:open).with(@file).and_return @file
        @file.stub!(:read).and_return @contents
      end
      it "should not be nil if the file does exist" do        
        @klass.include_deployment(@file).should_not == nil
      end
      it "should create a class called HankyDankyRanky" do
        @klass.include_deployment(@file).should_not == nil
        Object.const_defined?(:PoolPartyHankyDankyRankyClass).should == true
      end
      it "should create a method on the class called enable" do
        PoolPartyHankyDankyRankyClass.new.respond_to?(:enable).should == true
      end
      it "should add the contents of the file to be eval'd on the method" do
        PoolPartyHankyDankyRankyClass.new.enable.should == "hello goober"
      end
    end
  end
end