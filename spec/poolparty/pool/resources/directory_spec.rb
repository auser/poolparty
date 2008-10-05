require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "directory" do
  describe "instances" do
    before(:each) do
      @directory = directory({:name => "/etc/apache2/puppetmaster.conf"})
    end
    it "should turn the one hash instance into a string" do
      @directory.to_string.should =~ /"\/etc\/apache2\/puppetmaster\.conf":/
    end
    it "should turn the two hash instance into a string" do
      @directory = directory do
        name "/etc/init.d/puppetmaster"
        owner "redsmith"
      end
      @directory.to_string.should =~ /"\/etc\/init\.d\/puppetmaster":/
    end
    describe "as included" do            
      before(:each) do
        @directory = directory({:rent => "low"}) do
          name "/www/conf/httpd.conf"
        end
      end
      it "should use default values" do
        @directory.name.should == "/www/conf/httpd.conf"
      end
      it "should keep the default values for the directory" do
        @directory.mode.should == 644
      end
      it "should also set options through a hash" do
        @directory.rent.should == "low"
      end
      it "should have ensure set to directory" do
        @directory.ensure.should == "directory"
      end
    end
  end
end
