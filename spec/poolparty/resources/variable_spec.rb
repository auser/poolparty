require File.dirname(__FILE__) + '/../spec_helper'

describe "Variables" do
  describe "instances" do
    before(:each) do
      @tc = TestBaseClass.new do
        has_variable(:name => "master", :value => "herman")
      end
      @variable = @tc.resource(:variable).first
    end
    it "have the name in the options" do
      @variable.name.should == "master"
    end
    it "should store the owner's name as well" do
      @variable.value.should == "herman"
    end
    describe "into PuppetResolver" do
      before(:each) do
        @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
      end
      it "have the variable set in the puppet output" do
        @compiled.should match(/\$master = "herman"/)
      end
    end
  end
end
