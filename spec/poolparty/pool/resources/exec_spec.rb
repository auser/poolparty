require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Exec" do
  before(:each) do
    @exec = exec({:name => "/usr/bin/ifconfig"})
  end
  describe "instances" do
    it "should turn the one hash instance into a string" do
      @exec.to_string.should =~ /exec \{\n"\/usr\/bin\/ifconfig"/
    end
    it "should turn the two hash instance into a string" do
      @exec = exec({:name => "/usr/bin/ping 127.0.0.1"})
      @exec.to_string.should =~ /"\/usr\/bin\/ping 127\.0\.0\.1":/      
    end
    describe "as included" do            
      before(:each) do
        @exec = exec({:rent => "low", :ensures => "running"}) do
          name "/www/conf/httpd.conf"
        end
      end
      it "should use default values" do
        @exec.name.should == "/www/conf/httpd.conf"
      end
      it "should keep the default values for the exec" do
        @exec.path.should == "/usr/bin:/bin:/usr/local/bin"
      end
      it "should also set options through a hash" do
        @exec.rent.should == "low"
      end
      it "should ensure running, not the default 'present'" do
        @exec.ensure.should == "running"
      end
    end
  end
end
