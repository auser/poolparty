require File.dirname(__FILE__) + '/../../spec_helper'

describe "Capistrano provisioner" do
  before(:each) do
    @cloud = cloud :app do;end
    @remote_instance = PoolParty::Remote::RemoteInstance.new({:ip => "192.168.0.1", :status => "running", :name => "master"}, @cloud)
    stub_list_from_remote_for(@cloud)
  end
  describe "instance" do
    before(:each) do
      @pb = PoolParty::Provisioner::Capistrano.new(@remote_instance, @cloud)
    end
    it "should create the config on the initialize" do    
      @pb.config.class.should == ::Capistrano::Configuration
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
    describe "install tasks" do
      it "should have the configure tasks included" do
        @pb.master_install_tasks.include?("custom_configure_tasks").should == true
      end
    end
  end
end