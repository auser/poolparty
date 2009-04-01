require File.dirname(__FILE__) + '/../spec_helper'

describe "File" do
  describe "instances" do
    before(:each) do
      @tc = TestBaseClass.new do
        has_package(:name => "apache2")
      end
      @package = @tc.resource(:package).first
    end
    it "have the name in the options" do
      @package.name.should == "apache2"
    end
    it "should ensure it's present" do
      @package.ensures.should == "present"
    end
    describe "into PuppetResolver" do
      before(:each) do
        @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
      end
      it "should set the filename to the name of the file" do
        @compiled.should match(/package \{ "apache2"/)
      end
      it "have the mode set in the puppet output" do
        @compiled.should match(/ensure => "present"/)
      end
    end
  end
end
