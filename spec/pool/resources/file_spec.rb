require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "File" do
  before(:each) do
    @file = PoolParty::Resources::File.new
  end
  it "should have instances of files" do
    @file.respond_to?(:instances).should == true
  end
  describe "instances" do
    before(:each) do
      @instance1 = {:name => "/etc/apache2/puppetmaster.conf"}
      @file << @instance1
    end
    it "should turn the one hash instance into a string" do
      @file.to_s.should =~ /name => \/etc\/apache2\/puppetmaster\.conf;/      
    end
    it "should turn the two hash instance into a string" do
      @instance2 = {:name => "/etc/init.d/puppetmaster"}
      @file << @instance2
      @file.to_s.should =~ /name => \/etc\/apache2\/puppetmaster\.conf;/
    end
    describe "as included" do            
      before(:each) do
        @file = file do
          name "/www/conf/httpd.conf"
        end
      end
      it "should use default values" do
        @file.name.should == "/www/conf/httpd.conf"
      end
      it "should keep the default values for the file" do
        @file.mode.should == 644
      end
    end
  end
end
