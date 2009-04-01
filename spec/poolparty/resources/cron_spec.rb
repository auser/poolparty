require File.dirname(__FILE__) + '/../spec_helper'

describe "cron" do
  describe "instances" do
    before(:each) do
      @tc = TestBaseClass.new do
        has_cron("mail stats") do
          command "mail -s httpd.conf ari@poolpartyrb.com"
          hour 23
          minute 5
          weekday 1
        end
      end
      @cron = @tc.resource(:cron).first
    end
    it "have the name in the options" do
      @cron.name.should == "mail stats"
    end
    it "should store the owner's name as well" do
      @cron.command.should == "mail -s httpd.conf ari@poolpartyrb.com"
    end
    it "should store the time (from within the block)" do
      @cron.hour.should == 23
      @cron.minute.should == 5
      @cron.weekday.should == 1
    end
    describe "into PuppetResolver" do
      before(:each) do
        @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
      end
      it "should set the cronname to the name of the cron" do
        @compiled.should match(/cron \{ "mail stats"/)
      end
      it "set the command" do
        @compiled.should match(/command => "mail -s httpd\.conf ari@poolpartyrb\.com"/)
      end
      it "have the time set in the puppet output" do
        @compiled.should match(/hour => 23/)
        @compiled.should match(/minute => 5/)
        @compiled.should match(/weekday => 1/)
      end
    end
  end
end
