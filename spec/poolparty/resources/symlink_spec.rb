require File.dirname(__FILE__) + '/../spec_helper'

describe "Symlink" do
  describe "instances" do
    before(:each) do
      @tc = TestBaseClass.new do
        has_symlink("/etc/apache2/sites-enabled/poolpartyrb.com", :source => "/etc/apache2/sites-available/poolpartyrb.com")
      end
      @dir = @tc.resource(:symlink).first
    end
    it "have the name in the options" do
      @dir.name.should == "/etc/apache2/sites-enabled/poolpartyrb.com"
    end
    it "should store the source name" do
      @dir.source.should == "/etc/apache2/sites-available/poolpartyrb.com"
    end
    describe "into PuppetResolver" do
      before(:each) do
        @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
      end
      it "should set the filename to the name of the file" do
        @compiled.should match(/file \{ "\/etc\/apache2\/sites-enabled\/poolpartyrb\.com"/)
      end
      it "set the owner as the owner" do
        @compiled.should match(/ensure => "\/etc\/apache2\/sites-available\/poolpartyrb\.com"/)
      end
    end
  end
end
