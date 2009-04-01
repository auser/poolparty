require File.dirname(__FILE__) + '/../../spec_helper'

describe "Capistrano provisioner" do
  before(:each) do
    @cloud = new_test_cloud
    stub_remoter_for(@cloud)
  end
  describe "instance" do
    it "should be meaningfully spec'd" do
      pending
    end
    # before(:each) do
    #   @pb = PoolParty::Provisioner::Capistrano.new(@remote_instance, @cloud)
    # end
    # it "should have the cloud set as the cloud" do      
    #   @pb.cloud.should == @cloud
    # end
    # it "should create the config on the initialize" do    
    #   @pb.config.class.should == ::Capistrano::Configuration
    # end
    # describe "config" do
    #   it "should create the config at ::Capistrano::Logger::INFO if the cloud is verbose" do
    #     @cloud.stub!(:verbose).and_return true
    #     PoolParty::Provisioner::Capistrano.new(nil, @cloud).config.logger.level.should == ::Capistrano::Logger::INFO
    #   end
    #   it "should create the config at ::Capistrano::Logger::IMPORTANT if the cloud is not verbose" do
    #     @cloud.stub!(:verbose).and_return false
    #     PoolParty::Provisioner::Capistrano.new(nil, @cloud).config.logger.level.should == ::Capistrano::Logger::IMPORTANT
    #   end
    # end
    # describe "install tasks" do
    #   it "should have the configure tasks included" do
    #     @pb.master_install_tasks.include?("custom_configure_tasks").should == true
    #   end
    #   it "should call the copy_gem_bins_to_usr_bin method" do
    #     pending
    #   end
    # end
  end
end