require File.dirname(__FILE__) + '/../spec_helper'

describe "Script" do
  it "should have inflate as a class method" do
    Script.respond_to?(:inflate).should == true
  end
  it "should have inflate as an instance method" do
    Script.new.respond_to?(:inflate).should == true
  end
  
  describe "with a script" do
    before(:each) do
      @script = 'script'
      @filename = 'filename'

      @pool = Script.new
      Script.stub!(:new).and_return(@pool)
      @pool.stub!(:inflate).and_return true
    end
    
    it "should create a new Script when calling on the class method" do
      Script.should_receive(:new).and_return @pool      
    end    
    it "should instance eval the script" do
      @pool.should_receive(:instance_eval).with(@script, @filename).and_return true
    end
    it "should call inflate on itself" do
      @pool.should_receive(:inflate).and_return true
    end
    after do
      Script.inflate(@script, @filename)
    end
  end
end