require File.dirname(__FILE__) + '/../spec_helper'

describe "Remoter" do
  before(:each) do
    @cloud = cloud :app do;end
    @tc = TestClass.new
    # @tc.parent = @cloud
    @sample_instances_list = [{:ip => "192.168.0.1", :name => "master"}, {:ip => "192.168.0.2", :name => "node1"}]
  end
  describe "ssh_string" do
    it "should have the ssh command" do
      @tc.ssh_string.should =~ /ssh -o StrictHostKeyChecking=no -l/
    end
    it "should have the keypair in the ssh_string" do
      @tc.ssh_string.should =~ /#{@tc.full_keypair_path}/
    end
  end
  describe "ssh_array" do
    it "should have StrictHostKeyChecking set to no" do
      @tc.ssh_array.include?("-o StrictHostKeyChecking=no").should == true
    end
    it "should have the keypair path in the ssh_array" do
      @tc.ssh_array.include?('-i "'+@tc.full_keypair_path+'"').should == true
    end
  end
  describe "rsync_command" do
    before(:each) do
      @ri = Class.new
      @ri.stub!(:ip).and_return "192.168.0.22"
    end
    it "should have rsync in the rsync_command" do
      @tc.rsync_command.should == "rsync -azP --exclude cache -e '#{@tc.ssh_string} -l #{Default.user}'"
    end
    it "should be able to rsync storage commands" do
      @tc.rsync_storage_files_to_command(@ri).should == "#{@tc.rsync_command} #{Default.storage_directory}/ 192.168.0.22:/var/poolparty"
    end
  end

  # describe "launch_and_configure_master!" do
  #   before(:each) do
  #     @tc.stub!(:wait).and_return true
  #     stub_list_from_remote_for(@tc)
  #     stub_remoting_methods_for(@tc)
  #     @tc.stub!(:maximum_instances).and_return 5
  #     @tc.stub!(:list_of_pending_instances).and_return []
  #     @tc.stub!(:list_of_nonterminated_instances).and_return []
  #     @tc.stub!(:nodes(:status => "running")).and_return []
  #     @tc.stub!(:master).and_return ris.first
  #     @tc.stub!(:after_launched).and_return true
  #     @tc.stub!(:verbose).and_return false
  #     # @tc.stub!(:testing).and_return true #MF
  #     ::File.stub!(:exists?).and_return true
  #     
  #     @pb = PoolParty::Provisioner::Capistrano.new(nil, @tc)
  #     PoolParty::Provisioner::Capistrano.stub!(:new).and_return @pb
  #     @pb.stub!(:setup_runner)
  #     @pb.stub!(:install).and_return true
  #     @pb.stub!(:configure).and_return true
  #     @pb.stub!(:create_roles).and_return true
  #   end
  #   it "should have dependency_resolver_command" do
  #     @tc.respond_to?(:dependency_resolver_command)
  #     @tc.dependency_resolver_command.should match(/puppet/)
  #   end
  #   it "should have the method launch_master!" do
  #     @tc.respond_to?(:launch_and_configure_master!).should == true
  #   end
  #   it "should test if it can start a new instance" do
  #     @tc.should_receive(:can_start_a_new_instance?).once.and_return false      
  #   end
  #   it "should test if the master is running" do
  #     @tc.should_receive(:is_master_running?).and_return false
  #   end
  #   it "should ask to request_launch_new_instances when the master is not running and we can start a new instance" do
  #     @tc.should_receive(:request_launch_master_instance)
  #     @tc.stub!(:can_start_a_new_instance?).and_return true
  #     @tc.stub!(:is_master_running?).and_return false
  #   end
  #   after(:each) do
  #     @tc.launch_and_configure_master!
  #   end
  # end

  # TODO: Move to test unit
  # describe "expansions and contractions" do
  #   before(:each) do
  #     @tc = TestClass.new
  #     @tc.stub!(:nodes).and_return({:ip => "127.0.0.2", :status => "running"},
  #                                   {:ip => "127.0.0.2", :status => "running"})
  #     stub_running_remote_instances @tc
  #   end
  #   describe "list_of_nodes_exceeding_minimum_runtime" do
  #     before(:each) do
  #       @tc.stub!(:minimum_runtime).and_return 3000        
  #     end
  #     it "should not be empty" do
  #       @tc.nodes(:status => "running").size.should == 2
  #       @tc.nodes(:status => "running").first.elapsed_runtime.should be > 3000
  #       @tc.list_of_nodes_exceeding_minimum_runtime.size.should be > 0
  #     end
  #     it "should return a RemoteInstance" do
  #       @tc.list_of_nodes_exceeding_minimum_runtime.first.should be_instance_of(PoolParty::Remote::RemoteInstance)
  #     end
  #     it "are_any_nodes_exceeding_minimum_runtime? should be true" do
  #       @tc.are_any_nodes_exceeding_minimum_runtime?.should == true
  #     end
  #   end
  #   
  #   describe "are_too_few_instances_running?" do
  #     it "should be false if the number of running instances is larger than the minimum instances" do
  #       @tc.stub!(:minimum_instances).and_return 1
  #       @tc.are_too_few_instances_running?.should == false
  #     end
  #     it "should be true if the number of running instances is smaller than the minimum instances" do
  #       @tc.stub!(:minimum_instances).and_return 5
  #       @tc.are_too_few_instances_running?.should == true
  #     end
  #   end
  #   describe "are_too_many_instances_running?" do
  #     it "should be true if the number of running instances is larger than the maximum instances" do
  #       @tc.stub!(:maximum_instances).and_return 1
  #       p @tc.nodes
  #       @tc.are_too_many_instances_running?.should == true
  #     end
  #     it "should be false if the number of running instances is smaller than the maximum instances" do
  #       @tc.stub!(:maximum_instances).and_return 5
  #       @tc.are_too_many_instances_running?.should == false
  #     end
  #   end
  # end

end
