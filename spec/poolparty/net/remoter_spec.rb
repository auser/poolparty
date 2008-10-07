require File.dirname(__FILE__) + '/../spec_helper'

class TestClass
  include CloudResourcer
  include Remote
  using :ec2
  
  def keypair
    "fake_keypair"
  end
end
describe "Remoter" do
  before(:each) do
    @tc = TestClass.new
    ::File.stub!(:exists?).with("#{File.expand_path(Base.base_keypair_path)}/id_rsa-fake_keypair").and_return true
    @sample_instances_list = [{:ip => "192.168.0.1", :name => "master"}, {:ip => "192.168.0.2", :name => "node1"}]
  end
  describe "ssh_string" do
    it "should have the ssh command" do
      @tc.ssh_string.should =~ /ssh -o StrictHostKeyChecking=no -l '#{Base.user}' -i/
    end
    it "should have the keypair in the ssh_string" do
      @tc.ssh_string.should =~ /#{@tc.keypair}/
    end
  end
  describe "ssh_array" do
    it "should have StrictHostKeyChecking set to no" do
      @tc.ssh_array.include?("-o StrictHostKeyChecking=no").should == true
    end
    it "should have the user set to the base user class" do
      @tc.ssh_array.include?("-l '#{Base.user}'").should == true
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
      @tc.rsync_command.should == "rsync --delete -azP --exclude cache -e '#{@tc.ssh_string}'"
    end
    it "should be able to rsync storage commands" do
      @tc.rsync_storage_files_to_command(@ri).should == "#{@tc.rsync_command} #{Dir.pwd}/tmp 192.168.0.22:/var/poolparty"
    end
  end
  describe "listing" do
    before(:each) do
      @loc = Base.storage_directory + "/tc-instances.list"
      @locations = [@loc]
      @tc.stub!(:local_instances_list_file_locations).and_return @locations
    end
    it "should have the method list_from_local available" do
      TestClass.respond_to?(:list_from_local).should == true
    end
    it "should have the method list_from_remote available" do
      TestClass.respond_to?(:list_from_remote).should == true
    end
    describe "remote" do
      before(:each) do
        @tc.stub!(:list_of_instances).and_return @sample_instances_list
        @tc.stub!(:local_instances_list_file_locations).and_return [
          "#{Base.storage_directory}/tc-instances.list"
        ]
      end
      it "should call list_of_instances when trying to list from remote" do
        TestClass.should_receive(:list_of_instances).once.and_return @sample_instances_list
        TestClass.list_from_remote
      end
      it "should create a new RemoteInstance for each hashed instance" do
        PoolParty::Remote::RemoteInstance.should_receive(:new).exactly(2).and_return @ri
        @tc.list_from_remote
      end
      it "should return a string" do
        @tc.list_from_remote.class.should == Array
      end
      it "should contain the master in the listing" do
        @tc.list_from_remote.first.name.should == "master"
        @tc.list_from_remote.first.master?.should == true
      end
      it "should write to the first (preferred) local instances list file location for next time" do
        @tc.list_from_remote(:cache => true)
        ::File.file?(@tc.local_instances_list_file_locations.first).should == true
      end
      after(:all) do
        # Cleanup after ourselves
        # FileUtils.rm @loc if ::File.file?(@loc)
      end
    end
    describe "local" do
      describe "listing" do
        before(:each) do
          stub_list_from_remote_for(@tc)
        end
        it "should call local_instances_list_file_locations" do
          @tc.should_receive(:local_instances_list_file_locations).at_least(1).and_return [@loc]
        end
        it "should call File.file? on the local_instances_list_file_locations locations" do        
          File.should_receive(:file?).with(@loc).and_return false
        end
        it "should call get_working_listing_file to get the working local instance file" do
          @tc.should_receive(:get_working_listing_file).and_return nil
        end
      end
      describe "with listing" do
        before(:each) do
          str = "master 192.168.0.1
          node1 192.168.0.2"
          @loc.stub!(:read).and_return str
          TestClass.stub!(:open).and_return @loc
          TestClass.stub!(:get_working_listing_file).and_return @loc
          @ri = PoolParty::Remote::RemoteInstance.new({:ip => "192.168.0.1", :name => "master"})
          PoolParty::Remote::RemoteInstance.stub!(:new).and_return @ri
          
          stub_list_from_remote_for(@tc)
        end
        it "should call open on the get_working_listing_file" do
          @tc.should_receive(:open).with(@loc).at_least(1).and_return @loc
          @tc.list_from_local
        end
        it "should create a new RemoteInstance for each line in the file" do
          PoolParty::Remote::RemoteInstance.should_receive(:new).at_least(2)
          @tc.list_from_local
        end
        it "should return a string" do
          @tc.list_from_local.class.should == Array
        end
        it "should have the name of the master and the ip in the list_from_local" do
          @tc.list_from_local[0].name.should == "master"
        end
        it "should have name in the listing" do
          @ri.should_receive(:name).at_least(1).and_return "node0"
          @ri.to_s
        end
        it "should have the ip in the listing" do
          @ri.should_receive(:ip).at_least(1).and_return "127.0.0.1"
          @ri.to_s
        end
        it "should call to_s on the RemoteInstance instances" do          
          PoolParty::Remote::RemoteInstance.should_receive(:new).at_least(2).and_return @ri
          @ri.should_receive(:to_s).at_least(1)
          @ri.to_s
        end
      end
      after do
        hide_output do
          @tc.list_from_local
        end
      end
    end
  end
end