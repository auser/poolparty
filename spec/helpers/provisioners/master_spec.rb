require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/../../../lib/poolparty/helpers/provisioner'

include Provisioner

describe "Master provisioner" do
  before(:each) do
    @master = Master.new("127.0.0.1", :ubuntu)
  end
  describe "install_tasks" do
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
    @master.create_local_hosts_entry.should == "        echo \"127.0.0.1             puppet\" >> /etc/hosts\n"
  end
  it "should return create_basic_site_pp" do        
    @master.create_basic_site_pp.should == "        echo \"import 'nodes/*.pp'\" > /etc/puppet/manifests/site.pp\n        echo \"import 'classes/*.pp'\" >> /etc/puppet/manifests/site.pp\n        mkdir /etc/puppet/manifests/nodes /etc/puppet/manifests/classes\n"
  end
  it "should return setup_fileserver with the setup" do
    @master.setup_fileserver.should == "        echo \"[files]\n          path /data/puppet/fileserver\n          allow 127.0.0.1\" > /etc/puppet/fileserver.conf\n        mkdir -p /data/puppet/fileserver\n"
  end
  it "should be able to create_local_node" do
    @master.create_local_node.should == "        node \"master.127.0.0.1\" {\n           include hosts\n        }\n"    
  end
end