require File.dirname(__FILE__) + '/../spec_helper'
require "ftools"

module Hype
  def hyper
    "beatnick"
  end
  def instances_list
    []
  end
  register_remote_base :Hype
end

describe "Remote" do
  before(:each) do
    @cloud = cloud :test_cloud do;end
    
    @tc = TestClass.new
    @tc.stub!(:verbose).and_return false
    setup
  end
  it "should have the method 'using'" do
    @tc.respond_to?(:using).should == true
  end
  it "should include the module with using" do
    @tc.instance_eval do
      @remote_base = nil
    end
    @tc.should_receive(:extend).with("Hype".preserved_module_constant).once
    @tc.using :hype
  end
  it "should keep a list of the remote_bases" do
    @tc.stub!(:remote_bases).and_return [:ec2, :hype]
    @tc.available_bases.should == [:ec2, :hype]
  end
  it "should be able to register a new base" do
    @tc.remote_bases.should_receive(:<<).with(:hockey).and_return true
    @tc.register_remote_base("Hockey")
  end
  it "should not extend the module if the remote base isn't found" do
    @tc.should_not_receive(:extend)
    hide_output do
      @tc.using :paper
    end    
  end
  describe "when including" do
    it "should be able to say if it is using a remote base with using_remoter?" do
      @tc.using :hype
      @tc.using_remoter?.should_not == nil
    end
    it "should ask if it is using_remoter? when calling using" do
      @tc.should_receive(:using_remoter?).once
      @tc.using :hype
    end
    it "should only include the remote class once" do
      @tc.instance_eval do
        @remote_base = nil
      end
      @tc.should_receive(:extend).with(Hype).once
      @tc.using :hype
      @tc.using :hype
      @tc.using :hype
    end
  end
  describe "after using" do
    before(:each) do
      @tc = TestClass.new
      stub_list_from_remote_for(@tc, false)
      @tc.using :hype
    end
    it "should now have the methods available from the module" do
      @tc.respond_to?(:hyper).should == true
    end
    it "should raise an exception because the launch_new_instance! is not defined" do
      lambda {
        @tc.launch_new_instance!
      }.should raise_error
    end
    it "should not raise an exception because instances_list is defined" do
      lambda {
        @tc.remote_instances_list
      }.should_not raise_error
    end
    it "should run hyper" do
      @tc.hyper.should == "beatnick"
    end
  end
  describe "methods" do
    before(:each) do
      @tc = TestClass.new
      @tc.using :ec2
      
      @tc.reset!
      @tc.stub!(:minimum_instances).and_return 3
      @tc.stub!(:maximum_instances).and_return 5
      
      stub_list_from_remote_for(@tc)
      stub_list_of_instances_for(@tc)
    end
    describe "minimum_number_of_instances_are_running?" do
      it "should be false if there aren't" do
        @tc.minimum_number_of_instances_are_running?.should == false
      end
      it "should be true if there are" do
        add_stub_instance_to(@tc, 8)
        add_stub_instance_to(@tc, 9)
        add_stub_instance_to(@tc, 10)
        @tc.minimum_number_of_instances_are_running?.should == true
      end
    end
    describe "can_shutdown_an_instance?" do
      it "should say false because minimum instances are not running" do
        @tc.can_shutdown_an_instance?.should == false
      end
      it "should say we false if only the minimum instances are running" do
        add_stub_instance_to(@tc, 8)
        @tc.can_shutdown_an_instance?.should == false
      end
      it "should say true because the minimum instances + 1 are running" do
        add_stub_instance_to(@tc, 8)
        add_stub_instance_to(@tc, 9)
        @tc.can_shutdown_an_instance?.should == true
      end
    end
    describe "request_launch_new_instances" do
      it "should requiest to launch a new instance 3 times" do
        @tc.should_receive(:launch_new_instance!).exactly(3).and_return "launched"
        @tc.request_launch_new_instances(3)
      end
      it "should return a list of hashes" do
        @tc.should_receive(:launch_new_instance!).exactly(3).and_return({:instance_id => "i-dasfasdf"})
        @tc.request_launch_new_instances(3).first.class.should == Hash
      end
    end
    describe "can_start_a_new_instance?" do
      it "should be true because the maximum instances are not running" do
        @tc.can_start_a_new_instance?.should == false
      end
      it "should say that we cannot start a new instance because we are at the maximum instances" do
        add_stub_instance_to(@tc, 7)
        add_stub_instance_to(@tc, 8)
        add_stub_instance_to(@tc, 9)
        @tc.can_start_a_new_instance?.should == false
      end
    end
    describe "maximum_number_of_instances_are_not_running?" do
      it "should be true because the maximum are not running" do
        @tc.maximum_number_of_instances_are_not_running?.should == true
      end
      it "should be false because the maximum are running" do
        add_stub_instance_to(@tc, 7)
        add_stub_instance_to(@tc, 8)
        add_stub_instance_to(@tc, 9)
        @tc.maximum_number_of_instances_are_not_running?.should == false
      end
    end
    describe "request_launch_one_instance_at_a_time" do
      before(:each) do
        @tc.stub!(:wait).and_return "true"
        remove_stub_instance_from(@tc, 3)
        remove_stub_instance_from(@tc, 5)
        @tc.stub!(:launch_new_instance!).and_return {}
      end
      it "should call reset! once" do
        @tc.should_receive(:reset!).once
        @tc.request_launch_one_instance_at_a_time
      end
      it "should not call wait if there are no pending instances" do
        Kernel.should_not_receive(:sleep)
        @tc.request_launch_one_instance_at_a_time
      end
      # TODO: Stub methods with wait
    end
    describe "launch_minimum_number_of_instances" do
      it "should not call minimum_number_of_instances_are_running? if if cannot start a new instance" do
        @tc.stub!(:can_start_a_new_instance?).and_return false
        @tc.should_not_receive(:minimum_number_of_instances_are_running?)
        @tc.launch_minimum_number_of_instances
      end
      # TODO: Stub methods with wait
    end
    describe "request_termination_of_non_master_instance" do
      it "should reject the master instance from the list of instances (we should never shut down the master unless shutting down the cloud)" do
        @master = @tc.list_of_running_instances.select {|a| a.master? }.first
        @tc.should_not_receive(:terminate_instance!).with(@master).and_return true
        @tc.request_termination_of_non_master_instance
      end
      it "should call terminate on an instance" do
        @tc.should_receive(:terminate_instance!).once
        @tc.request_termination_of_non_master_instance
      end
    end
    describe "expansions" do
      before(:each) do
        setup
        @tc.stub!(:copy_ssh_app).and_return true
        @tc.stub!(:prepare_reconfiguration).and_return "full"
        PoolParty::Provisioner.stub!(:reconfigure_master).and_return true
        @tc.stub!(:wait).and_return true
        @tc.stub!(:nonmaster_nonterminated_instances).and_return response_list_of_instances
        @inst = stub_instance(9, "running")
        @tc.nonmaster_nonterminated_instances.stub!(:last).and_return @inst
        @inst.stub!(:options).and_return({:name => "red"})
        @tc.stub!(:rsync_storage_files_to).and_return true
        @tc.stub!(:run_command_on).and_return true
        @tc.stub!(:full_keypair_path).and_return "true"
      end
      describe "expand_cloud_if_necessary" do
        before(:each) do
          setup
          stub_list_from_remote_for(@tc)
          @ri = PoolParty::Remote::RemoteInstance.new(:ip => "127.0.0.1", :num => 1, :name => "master")
          @tc.stub!(:request_launch_new_instances).and_return @ri
          @tc.stub!(:can_start_a_new_instance).and_return true
          @tc.stub!(:list_of_pending_instances).and_return []
          @tc.stub!(:prepare_for_configuration).and_return true
          @tc.stub!(:build_and_store_new_config_file).and_return true          
          PoolParty::Provisioner.stub!(:provision_slaves).and_return true
          @cloud.stub!(:master).and_return @ri
          @cloud.stub!(:list_of_nonterminated_instances).and_return [@ri]
          @cloud.stub!(:full_keypair_path).and_return "keyairs"
          
          @provisioner = PoolParty::Provisioner::Capistrano.new(@ri, @cloud, :ubuntu)
          PoolParty::Provisioner::Capistrano.stub!(:new).and_return @provisioner
          @provisioner.stub!(:install).and_return true
          @provisioner.stub!(:configure).and_return true
        end
        it "should receive can_start_a_new_instance?" do
          @tc.should_receive(:can_start_a_new_instance?).once
        end
        it "should see if we should expand the cloud" do
          @tc.should_receive(:should_expand_cloud?).once.and_return false
        end
        it "should call request_launch_new_instances if we should_expand_cloud?" do
          @tc.should_receive(:should_expand_cloud?).once.and_return true
          @tc.should_receive(:request_launch_one_instance_at_a_time).once.and_return [{:ip => "127.0.0.5", :name => "node2"}]
        end
        it "should call a new slave provisioner" do
          @tc.stub!(:should_expand_cloud?).once.and_return true
          @provisioner.should_receive(:install).at_least(1)
        end
        after(:each) do
          @tc.expand_cloud_if_necessary
        end
      end
      describe "contract_cloud_if_necessary" do
        before(:each) do
          @tc.stub!(:request_termination_of_non_master_instance).and_return true
          @tc.stub!(:are_any_nodes_exceeding_minimum_runtime?).and_return true
          @tc.stub!(:wait).and_return true
          @tc.stub!(:valid_rules?).and_return false
          @tc.stub!(:can_shutdown_an_instance?).and_return true
        end
        it "should receive can_shutdown_an_instance?" do
          @tc.should_receive(:can_shutdown_an_instance?).once
        end
        it "should see if we should contract the cloud" do
          @tc.should_receive(:should_contract_cloud?).once.and_return false
        end
        it "should call request_termination_of_non_master_instance if we should_contract_cloud?" do
          @tc.stub!(:should_contract_cloud?).and_return true
          @tc.should_receive(:request_termination_of_non_master_instance).once.and_return true
        end
        after(:each) do
          @tc.contract_cloud_if_necessary
        end
      end
    end
    describe "rsync_storage_files_to" do
      before(:each) do
        Kernel.stub!(:system).and_return true
        @tc.extend CloudResourcer        
        @tc.stub!(:keypair_path).and_return "~/.ec2/fake_keypair"
        @obj = Object.new
        @obj.stub!(:ip).and_return "192.168.0.1"
      end
      it "should call exec on the kernel" do
        @tc.stub!(:keypair).and_return "funky"
        ::File.stub!(:exists?).with("#{File.expand_path(Base.base_keypair_path)}/id_rsa-funky").and_return true
        lambda {
          @tc.rsync_storage_files_to(@tc.master)
        }.should_not raise_error
      end
      describe "run_command_on" do
        before(:each) do
          @tc.stub!(:keypair).and_return "fake_keypair"
          @tc.stub!(:keypair_path).and_return "~/.ec2/fake_keypair"
          @obj.stub!(:name).and_return "pop"
        end
        it "should call system on the kernel" do
          ::File.stub!(:exists?).with("#{File.expand_path(Base.base_keypair_path)}/id_rsa-funky").and_return true
          Kernel.should_receive(:system).with("#{@tc.ssh_string} 192.168.0.1 'ls'").and_return true
          @tc.run_command_on("ls", @obj)
        end
      end
      describe "ssh_into_instance_number" do
        before(:each) do          
          @tc.stub!(:keypair).and_return "fake_keypair"
          ::File.stub!(:exists?).with("#{File.expand_path(Base.base_keypair_path)}/id_rsa-funky").and_return true
          
          Kernel.stub!(:system).with("#{@tc.ssh_string} 192.168.0.1").and_return true          
        end
        it "should find the instance" do
          @tc.should_receive(:get_instance_by_number).with(0).and_return @obj
          @tc.ssh_into_instance_number
        end
      end
    end
  end
end