require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/../../../../lib/poolparty/helpers/provisioner_base'

include Provisioner

describe "Slave provisioner" do
  before(:each) do
    
    @cloud = cloud :app do; end
    @remote_instance = PoolParty::Remote::RemoteInstance.new({:ip => "192.168.0.1", :status => "running", :name => "master"}, @cloud)
    stub_list_from_remote_for(@cloud)
    
    @cloud.stub!(:master).and_return @ris.first
    
    @slave = Slave.new(@remote_instance, @cloud, :ubuntu)
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
    @slave.install_puppet.should =~ /        apt-get install -y puppet puppetmaster/
  end
end