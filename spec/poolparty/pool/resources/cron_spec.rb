require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Cron" do
  describe "instances" do
    before(:each) do
      @cron = cron({:command => "/bin/logrotate"})
    end
    it "should turn the one hash instance into a string" do
      @cron.to_string.should =~ /'\/bin\/logrotate'/
    end
    it "should turn the two hash instance into a string" do
      @cron = cron({:name => "mail", :command => "/bin/mail -s \"letters\""})
      @cron.to_string.should =~ /"mail":/
    end
    describe "as included" do            
      before(:each) do
        @cron = cron({:rent => "low"}) do
          name "/www/conf/httpd.conf"
          hour 23
          minute 5
          weekday 1
        end
      end
      it "should use default values" do
        @cron.name.should == "/www/conf/httpd.conf"
      end
      it "should keep the default values for the file" do
        @cron.user.should == "root"
      end
      it "should also set options through a hash" do
        @cron.rent.should == "low"
      end
      it "should set the hour to 23" do
        @cron.hour.should == 23
      end
      it "should set the minute to 5" do
        @cron.minute.should == 5
      end
      it "should set the weekday to 1" do
        @cron.weekday.should == 1
      end
    end
  end
end