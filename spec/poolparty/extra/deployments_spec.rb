#TODO: Reimplement
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
        @klass.include_deployment(@file).should_not == nil
      end
      it "should not be nil if the file does exist" do        
        @klass.include_deployment(@file).should_not == nil
      end
      it "should create a class called PoolPartyHankyDankyRankyClass" do
        @klass.include_deployment(@file).nil?.should == false
        Object.const_defined?(:HankyDankyRankyClass).should == true
      end
      it "should create a method on the class called enable" do
        HankyDankyRankyClass.new.respond_to?(:enable).should == true
      end
      it "should add the contents of the file to be eval'd on the method" do
        # puts HankyDankyRankyClass.new.enable
        # HankyDankyRankyClass.new.enable.should == "hello goober"
        pending
      end
    end
    describe "include_deployments" do
      before(:each) do
        ::File.stub!(:directory?).and_return false
        @dir = "/deployments"
      end
      it "should have the method include_deployments" do
        @klass.respond_to?(:include_deployments).should == true
      end
      it "should return nil if the directory does not exist" do
        @klass.include_deployments(@dir).should == nil
      end
      describe "existing directory" do
        before(:each) do          
          ::File.stub!(:directory?).and_return true
          @contents = []
          Dir.stub!(:[]).and_return @contents
        end
        it "should not be nil if the directory exists" do
          @klass.include_deployments(@dir).nil?.should == false
        end
        it "should call Dir.[] on the directory" do
          Dir.should_receive(:[]).with("#{@dir}/*").and_return []
          @klass.include_deployments(@dir)
        end
      end
    end
  end
end