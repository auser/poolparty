require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/../../../lib/poolparty/helpers/provisioner'

include Provisioner

describe "Master provisioner" do
  before(:each) do
    @master = Master.new(:ubuntu)
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
    after do
      @master.install
    end
  end
  it "should return install_puppet_master as apt-get install puppet factor" do
    @master.install_puppet_master.should == "apt-get install puppet factor"
  end
  it "should return create_local_hosts_entry as echo" do
    @master.create_local_hosts_entry.should == "        echo \"ubuntu             puppet\" >> /etc/hosts\n"
  end
  it "should return create_basic_site_pp" do    
    @master.create_basic_site_pp.should == "        echo \"import 'nodes/*.pp'\" > /etc/puppet/manifests/site.pp\n        echo \"import 'classes/*.pp'\" >> /etc/puppet/manifests/site.pp\n"
  end
end