require File.dirname(__FILE__) + '/../../spec_helper'

include Provisioner

describe "Master provisioner" do
  before(:each) do
    @cloud = cloud :app do; end
    stub_list_from_remote_for(@cloud)
    @remote_instance = PoolParty::Remote::RemoteInstance.new({:ip => "192.168.0.1", :status => "running", :name => "master"}, @cloud)
    
    @cloud.stub!(:master).and_return @ris.first
    @master = Master.new(@cloud, :ubuntu)
  end
  describe "install_tasks" do
    before(:each) do
      @cloud.stub!(:master).and_return @ris.first
      @master.stub!(:cloud).and_return @cloud
    end
    it "should call install_puppet_master" do
      @master.should_receive(:install_puppet)
    end
    it "should call create_local_hosts_entry" do
      @master.should_receive(:create_local_hosts_entry)
    end
    it "should call setup_fileserver" do
      @master.should_receive(:setup_fileserver)
    end
    it "should call create_local_node" do
      @master.should_receive(:create_local_node).twice
    end
    it "should call the custom_install_tasks" do
      @master.should_receive(:custom_install_tasks)
    end
    after do
      @master.install
    end
  end
  it "should return install_puppet_master as apt-get install puppet factor" do
    @master.install_puppet.should =~ /install -y puppet puppetmaster/
  end
  it "should return setup basic structure and set classes into the manifest" do
    @master.setup_basic_structure.should =~ /echo "import 'classes\/\*\.pp'" >> \/etc\/puppet\/manifests\/site\.pp/
  end
  it "should return setup_fileserver with the setup" do
    @master.setup_fileserver.should =~ /\[files\]/
  end
  it "should be able to create_local_node" do
    @master.create_local_node.should =~ /ode \"master\"/
  end
  it "should create a node1 node as well" do
    @master.create_local_node.should =~ /ode \"node1\"/
  end
end