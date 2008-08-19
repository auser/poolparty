require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "File" do
  before(:each) do
    @cron = PoolParty::Resources::Cron.new
  end
  it "should have instances of files" do
    @cron.respond_to?(:instances).should == true
  end
  describe "instances" do
    before(:each) do
      @instance1 = {:command => "/bin/logrotate"}
      @cron << @instance1
    end
    it "should turn the one hash instance into a string" do
      @cron.to_s.should =~ /command => '\/bin\/logrotate';/
    end
    it "should turn the two hash instance into a string" do
      @instance2 = {:name => "/bin/mail -s \"letters\""}
      @cron << @instance2
      @cron.to_s.should =~ /\/bin\/mail -s \"letters\":/
    end
    describe "as included" do            
      before(:each) do
        @cron = file({:rent => "low"}) do
          name "/www/conf/httpd.conf"
        end
      end
      it "should use default values" do
        @cron.name.should == "/www/conf/httpd.conf"
      end
      it "should keep the default values for the file" do
        @cron.mode.should == 644
      end
      it "should also set options through a hash" do
        @cron.rent.should == "low"
      end
    end
  end
end
