require File.dirname(__FILE__) + '/../spec_helper'

describe "Service" do
  describe "instances" do
    before(:each) do
      @tc = TestBaseClass.new do
        has_service("apache2", {:hasrestart => true})
      end
      @service = @tc.resource(:service).first
    end
    it "have the name in the options" do
      @service.name.should == "apache2"
    end
    it "should store the hasrestart option" do
      @service.hasrestart.should == true
    end
    describe "into PuppetResolver" do
      before(:each) do
        @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
      end
      it "should set the filename to the name of the file" do
        @compiled.should match(/service \{ "apache2"/)
      end
      it "have the mode set in the puppet output" do
        @compiled.should match(/hasrestart => true/)
      end
    end
  end
end
