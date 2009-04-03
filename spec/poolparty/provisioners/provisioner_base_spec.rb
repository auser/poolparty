# TODO: Deprecate

# require File.dirname(__FILE__) + '/../spec_helper'
# 
# include Provisioner
# 
# describe "ProvisionerBase" do
#   before(:each) do    
#     ::Suitcase::Zipper.stub!(:gems).and_return true
#     # ::Suitcase::Zipper.stub!(:packages).and_return true
#     @cloud = new_test_cloud
#     @remote_instance = PoolParty::Remote::RemoteInstance.new({:ip => "192.168.0.1", :status => "running", :name => "master"}, @cloud)
#     @pb = PoolParty::Provisioner::ProvisionerBase.new(@remote_instance, @cloud)
#     stub_list_from_remote_for(@cloud)
#     stub_remoting_methods_for(@cloud)
#     Kernel.stub!(:sleep).and_return true
#     @cloud.stub!(:when_no_pending_instances).and_return true
#     @cloud.stub!(:when_all_assigned_ips).and_return true
#   end
#   describe "class methods" do
#     it "should have install" do
#       ProvisionerBase.respond_to?(:install).should == true
#     end
#     it "should have configure" do
#       ProvisionerBase.respond_to?(:configure).should == true
#     end
#     describe "in action" do
#       it "should call a new ProvisionerBase" do
#         ProvisionerBase.should_receive(:new).with(@remote_instance, @cloud).and_return @pb
#         @cloud.stub!(:remote_instances_list).and_return sample_instances_list
#         ProvisionerBase.install(@remote_instance, @cloud)
#       end
#     end
#   end
#   describe "instance methods" do
#     before(:each) do
#       @pb = PoolParty::Provisioner::ProvisionerBase.new(@remote_instance, @cloud)
#       stub_list_from_remote_for(@pb)
#       stub_list_from_remote_for(@cloud)
#       @cloud.stub!(:remote_instances_list).and_return sample_instances_list
#       stub_remoting_methods_for(@pb)
#     end
#     it "should store the instance on the ProvisionerBase" do
#       @pb.instance.should == @remote_instance
#     end
#     it "should store the cloud on the ProvisionerBase" do
#       @pb.cloud.should == @cloud
#     end
#     it "should say provision_master? is true if the remote instance name is master" do
#       @pb.provision_master?.should == true
#     end
#     it "should say the provision_master? is false if the remote instance is not the master" do
#       @remote_instance.stub!(:master?).and_return false
#       @pb.provision_master?.should == false
#     end
#     it "should say the provision_master? is false if the remote instance is nil" do
#       PoolParty::Provisioner::ProvisionerBase.new(nil, @cloud).provision_master?.should == false
#     end
#     describe "custom tasks" do
#       it "should call custom_configure_tasks_for on the cloud with the instance" do
#         @cloud.should_receive(:custom_configure_tasks_for).with(@remote_instance).and_return []
#         @pb.custom_configure_tasks
#       end
#       it "should call custom_install_tasks_for on the cloud with the instance" do
#         @cloud.should_receive(:custom_install_tasks_for).with(@remote_instance).and_return []
#         @pb.custom_install_tasks
#       end
#     end
#     describe "installation" do
#       it "should have an install method" do
#         @pb.respond_to?(:install).should == true
#       end
#       it "should call error if it is not valid" do
#         @pb.stub!(:valid?).and_return false
#         lambda {@pb.install}.should raise_error
#       end
#       it "should not raise an exception if it is valid" do
#         @pb.stub!(:valid?).and_return true
#         lambda {@pb.install(true)}.should_not raise_error
#       end
#       it "should call before_install with the instance" do
#         @pb.should_receive(:before_install).with(@remote_instance).and_return true
#         @pb.install
#       end
#       it "should call setup_runner" do
#         @pb.should_receive(:setup_runner).and_return true
#         @pb.install
#       end
#       it "should call process_install! with the testing" do
#         @pb.should_receive(:process_install!).with(false).and_return true
#         @pb.install
#       end
#       it "should call after_install with the instance" do
#         @pb.should_receive(:after_install).with(@remote_instance).and_return true
#         @pb.install
#       end
#     end
#     describe "configuration" do
#       it "should have a configure method" do
#         @pb.respond_to?(:configure).should == true
#       end
#       it "should call error if it is not valid" do
#         @pb.stub!(:valid?).and_return false
#         lambda {@pb.configure}.should raise_error
#       end
#       it "should not raise an exception if it is valid" do
#         @pb.stub!(:valid?).and_return true
#         lambda {@pb.configure(true)}.should_not raise_error
#       end
#       it "should call before_configure with the instance" do
#         @pb.should_receive(:before_configure).with(@remote_instance).and_return true
#         @pb.configure
#       end
#       it "should call setup_runner" do
#         @pb.should_receive(:setup_runner).and_return true
#         @pb.configure
#       end
#       it "should call process_configure! with the testing" do
#         @pb.should_receive(:process_configure!).with(false).and_return true
#         @pb.configure
#       end
#       it "should call after_configure with the instance" do
#         @pb.should_receive(:after_configure).with(@remote_instance).and_return true
#         @pb.configure
#       end
#       
#     end
#   end
# end