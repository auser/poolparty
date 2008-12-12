require File.dirname(__FILE__) + '/../../spec_helper'

describe "Capistrano provisioner" do
  before(:each) do
    @cloud = cloud :app do;end
    @remote_instance = PoolParty::Remote::RemoteInstance.new({:ip => "192.168.0.1", :status => "running", :name => "master"}, @cloud)
    stub_list_from_remote_for(@cloud)
  end
  it "should subclass ProvisionerBase" do
    PoolParty::Provisioner::Capistrano.ancestors[1].should == PoolParty::Provisioner::ProvisionerBase
  end
  describe "instance" do
    before(:each) do
      @pb = PoolParty::Provisioner::Capistrano.new(@remote_instance, @cloud)
    end
    it "should create the config on the initialize" do    
      @pb.config.class.should == Capistrano::Configuration
    end
    it "should create the role as master because the remote instance is the master" do
      @pb.roles.should == [:master]
    end
    it "should create the role as the slaves if the remote instance is nil" do
      PoolParty::Provisioner::Capistrano.new(nil, @cloud).roles.should == [:slaves]
    end
    it "should create the role as the slaves if the remote instance is not the master" do
      @remote_instance.stub!(:master?).and_return false
      @pb.roles.should == [:master]
    end
    describe "config" do
      it "should create the config at ::Capistrano::Logger::INFO if the cloud is verbose" do
        @cloud.stub!(:verbose).and_return true
        PoolParty::Provisioner::Capistrano.new(nil, @cloud).config.logger.level.should == ::Capistrano::Logger::INFO
      end
      it "should create the config at ::Capistrano::Logger::IMPORTANT if the cloud is not verbose" do
        @cloud.stub!(:verbose).and_return false
        PoolParty::Provisioner::Capistrano.new(nil, @cloud).config.logger.level.should == ::Capistrano::Logger::IMPORTANT
      end      
    end
  end
end