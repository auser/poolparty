require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Conditional" do
  before(:each) do
    @cloud = cloud :app do; end
    @cloud.instance_eval do
      execute_if("$hostname", "'master'", self) do
        file({:name => "/etc/apache2/puppetmaster.conf"})
      end
    end
    @cond = @cloud.resource(:conditional).first
  end
  it "should add the block of resources on the parent" do
    @cloud.resources.size.should == 1
  end
  it "should have a conditional in the resources" do
    @cloud.resource(:conditional).first.name.should == "$hostname == 'master'"
  end
  it "should push the resources onto the conditional resource" do
    @cond.resources.size.should == 1
  end
  it "should have a file resource on the conditional" do
    @cond.resource(:file).first.name.should == "/etc/apache2/puppetmaster.conf"
  end
  it "should have the parent as the cloud" do
    @cond.parent.should == @cloud
  end
  describe "to_string" do
    before(:each) do
      @string = @cond.to_string
    end
    it "should have a case statement for the hostname" do
      @string.should =~ /case \$hostname/
    end
  end
end