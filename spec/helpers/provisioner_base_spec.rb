require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/../../lib/poolparty/helpers/provisioner'

include Provisioner

describe "ProvisionerBase" do
  it "should respond to the class method install" do
    ProvisionerBase.respond_to?(:install).should == true
  end
  it "should return a hash when asking for the installers" do
    ProvisionerBase.installers.class.should == Hash
  end
  it "should not return an empty string when asking for the Ubuntu installer" do
    ProvisionerBase.installers[:ubuntu].should_not be_nil
  end
  it "should be able to fetch the ubuntu installer with the helper method installer" do
    ProvisionerBase.installer("ubuntu").should == "apt-get"
  end
end