require File.dirname(__FILE__) + '/../../spec_helper'

include Provisioner

describe "Slave provisioner" do
  before(:each) do
    
    @cloud = cloud :app do; end
    @remote_instance = PoolParty::Remote::RemoteInstance.new({:ip => "192.168.0.1", :status => "running", :name => "master"}, @cloud)
    stub_list_from_remote_for(@cloud)
    
    @cloud.stub!(:master).and_return sample_instances.first
    
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
  it "should return setup_puppet with the master" do
    @slave.setup_puppet.should =~ /master/
  end
end