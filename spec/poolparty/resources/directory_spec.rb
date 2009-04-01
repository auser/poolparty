require File.dirname(__FILE__) + '/../spec_helper'

describe "Directory" do
  describe "instances" do
    before(:each) do
      @tc = TestBaseClass.new do
        has_directory({:name => "/etc/apache2", :owner => "herman"}) do
          mode 755
        end
      end
      @dir = @tc.resource(:directory).first
    end
    it "have the name in the options" do
      @dir.name.should == "/etc/apache2"
    end
    it "should store the owner's name" do
      @dir.owner.should == "herman"
    end
    it "should store the mode (from within the block)" do
      @dir.mode.should == 755
    end
    describe "into PuppetResolver" do
      before(:each) do
        @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
      end
      it "should set the filename to the name of the file" do
        @compiled.should =~ /file \{ "\/etc\/apache2"/
      end
      it "set the owner as the owner" do
        @compiled.should =~ /owner => "herman"/
      end
      it "should say it's a directory in the ensure method" do
        @compiled.should =~ /ensure => "directory"/
      end
      it "have the mode set in the puppet output" do
        @compiled.should =~ /mode => 755/
      end
    end
  end
end
