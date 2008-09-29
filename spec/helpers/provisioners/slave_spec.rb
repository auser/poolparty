require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/../../../lib/poolparty/helpers/provisioner_base'

include Provisioner

describe "Slave provisioner" do
  before(:each) do
    @slave = Slave.new("127.0.0.5", "127.0.0.1", :ubuntu)
  end
  describe "install_tasks" do
    it "should call install_puppet_slave" do
      @slave.should_receive(:install_puppet)
    end
    after do
      @slave.install
    end
  end
  it "should return install_puppet as apt-get install puppet factor" do
    @slave.install_puppet.should == "apt-get install puppet factor"
  end
end