require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "File" do
  describe "instances" do
    before(:each) do
      @file = file({:name => "/etc/apache2/puppetmaster.conf"})
    end
    it "should turn the one hash instance into a string" do
      @file.to_string.should =~ /"\/etc\/apache2\/puppetmaster\.conf":/
    end
    it "should turn the two hash instance into a string" do
      @file = file do
        name "/etc/init.d/puppetmaster"
        owner "redsmith"
      end
      @file.to_string.should =~ /"\/etc\/init\.d\/puppetmaster":/
    end
    describe "as included" do            
      before(:each) do
        @file = file({:rent => "low"}) do
          name "/www/conf/httpd.conf"
        end
      end
      it "should use default values" do
        @file.name.should == "/www/conf/httpd.conf"
      end
      it "should keep the default values for the file" do
        @file.mode.should == 644
      end
      it "should also set options through a hash" do
        @file.rent.should == "low"
      end
    end
  end
end
