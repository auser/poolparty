require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/../../../lib/poolparty/helpers/provisioner_base'

include Provisioner

describe "Master provisioner" do
  before(:each) do
    @cloud = cloud :app do; end
    stub_list_from_local_for(@cloud)    
    @cloud.stub!(:master).and_return @ris.first
    
    @master = Master.new(@cloud, :ubuntu)
  end
  describe "install_tasks" do
    before(:each) do
      @cloud.stub!(:master).and_return @ris.first
      @master.stub!(:cloud).and_return @cloud
    end
    it "should call install_puppet_master" do
      @master.should_receive(:install_puppet_master)
    end
    it "should call create_local_hosts_entry" do
      @master.should_receive(:create_local_hosts_entry)
    end
    it "should call create_basic_site_pp" do
      @master.should_receive(:create_basic_site_pp)
    end
    it "should call setup_fileserver" do
      @master.should_receive(:setup_fileserver)
    end
    it "should call create_local_node" do
      @master.should_receive(:create_local_node)      
    end
    after do
      @master.install
    end
  end
  it "should return install_puppet_master as apt-get install puppet factor" do
    @master.install_puppet_master.should == "apt-get install puppet factor"
  end
  it "should return create_local_hosts_entry as echo" do
    @master.create_local_hosts_entry.should == "        echo \"192.168.0.1             puppet master\" >> /etc/hosts\n"
  end  
  it "should return create_basic_site_pp" do        
    @master.create_basic_site_pp.should =~ /echo \"node default \{\n/
  end
  it "should return setup basic structure" do
    @master.setup_basic_structure.should =~ /puppetmasterd --mkusers/
  end
  it "should return setup_fileserver with the setup" do
    @master.setup_fileserver.should == "        echo \"[files]\n          path /data/puppet/fileserver\n          allow 192.168.0.1\" > /etc/puppet/fileserver.conf\n        mkdir -p /data/puppet/fileserver\n"
  end
  it "should be able to create_local_node" do
    @master.create_local_node.should =~ /ode \"master\" \{\}/
  end
  it "should create a node1 node as well" do
    @master.create_local_node.should =~ /ode \"node1\" \{\}/
  end
end