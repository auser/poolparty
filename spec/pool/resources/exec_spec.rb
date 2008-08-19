require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Exec" do
  before(:each) do
    @instance1 = {:name => "/usr/bin/ifconfig"}
    @exec = PoolParty::Resources::Exec.new(@instance1)
  end
  it "should have instances of execs" do
    @exec.respond_to?(:instances).should == true
  end
  describe "instances" do
    before(:each) do      
      @exec << @instance1
    end
    it "should turn the one hash instance into a string" do
      @exec.to_s.should =~ /exec \{\n\t\/usr\/bin\/ifconfig/
    end
    it "should turn the two hash instance into a string" do
      @instance2 = {:name => "/usr/bin/ping 127.0.0.1"}
      @exec << @instance2
      @exec.to_s.should =~ /\n\t\/usr\/bin\/ping 127\.0\.0\.1:/
    end
    describe "as included" do            
      before(:each) do
        @exec = exec({:rent => "low"}) do
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
    end
  end
end
