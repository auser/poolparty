require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/../../../lib/poolparty/helpers/provisioner_base'

include Provisioner

describe "ProvisionerBase" do
  before(:each) do    
    @cloud = cloud :app do;end    
    @remote_instance = PoolParty::Remote::RemoteInstance.new({:ip => "192.168.0.1", :status => "running", :name => "master"}, @cloud)
    @cloud.stub!(:custom_install_tasks_for).with(@remote_instance).and_return []
    stub_list_from_remote_for(@remote_instance)
    stub_list_from_remote_for(@cloud)
  end
  describe "class methods" do
    it "should have install" do
      ProvisionerBase.respond_to?(:install).should == true
    end
    it "should have configure" do
      ProvisionerBase.respond_to?(:configure).should == true
    end
  end
  describe "tasks" do
    before(:each) do
      class Provisioner::TestProvisioner < Provisioner::ProvisionerBase
        def tasks
          [
            "hello",
            " ",
            "world"
          ]          
        end
      end
      @tp = TestProvisioner.new(@remote_instance,@cloud)
    end
    it "should have tasks on the provisioner" do
      ProvisionerBase.new(@remote_instance,@cloud).install_tasks.class.should == Array
    end
    it "should have empty tasks on the provisioner" do
      ProvisionerBase.new(@remote_instance,@cloud).install_tasks.should be_empty
    end
    it "should allow a new class to write tasks that aren't empty upon instantiation" do
      @tp.tasks.should_not be_empty
    end
    it "should have described 3 tasks in the task_list" do
      @tp.tasks.size.should == 3
    end
  end
  it "should return a hash when asking for the installers" do
    ProvisionerBase.installers.class.should == Hash
  end
  it "should not return an empty string when asking for the Ubuntu installer" do
    ProvisionerBase.installers[:ubuntu].should_not be_nil
  end
  it "should be able to fetch the ubuntu installer with the helper method installer" do
    ProvisionerBase.new(@remote_instance,@cloud, "ubuntu").installer_for.should == "aptitude install -y "
  end
  it "should be able to fetch the fedora installer with the helper method installer" do
    ProvisionerBase.new(@remote_instance,@cloud, "fedora").installer_for.should == "yum install "
  end
  describe "install_string" do
    before(:each) do
      class Provisioner::BTestProvisioner < Provisioner::ProvisionerBase
        def install_tasks
          [
            "hello",
            insert_space,
            add_world
          ]          
        end
        def insert_space
          " "
        end
        def add_world
          "cruel world"
        end
        def default_install_tasks
          [] << install_tasks
        end
      end
      @provisioner = BTestProvisioner.new(@remote_instance, @cloud)
    end
    it "should not run \"hello\"" do
      @provisioner.should_not_receive(:hello)
    end
    it "should run insert_space" do
      @provisioner.should_receive(:insert_space).once.and_return " "
    end
    it "should run add_world" do
      @provisioner.should_receive(:insert_space).once.and_return "world"
    end
    it "should compound the string to be hello \n   \n cruel world" do
      @provisioner.install_string.should == "hello \n   \n cruel world"
    end
    it "should call the last_install_tasks last" do
      @provisioner.should_receive(:last_install_tasks).at_least(1).and_return ["pops"]
    end
    it "should append the last_install_tasks to the end" do
      @provisioner.stub!(:last_install_tasks).and_return ["pops"]
      @provisioner.install_string.should == "hello \n   \n cruel world \n pops"
    end
    after do
      @provisioner.install_string
    end
    describe "processing" do
      before(:each) do
        @provisioner = ProvisionerBase.new(@remote_instance, @cloud)
        stub_list_from_remote_for(@cloud)
        @cloud.stub!(:keypair).and_return "fake_keypair"
        @cloud.stub!(:keypair_path).and_return "~/.ec2/fake_keypair"
        @cloud.stub!(:other_clouds).and_return []
        @cloud.stub!(:expand_when).and_return "cpu > 10"
        @cloud.stub!(:copy_file_to_storage_directory).and_return true
        @cloud.stub!(:rsync_storage_files_to).and_return true
        @cloud.stub!(:minimum_runnable_options).and_return []
        Provisioner::Master.stub!(:new).and_return @provisioner
        @provisioner.stub!(:build_and_store_new_config_file).and_return true
        @provisioner.stub!(:process_clean_reconfigure_for!).and_return true
      end
      describe "provision_master" do
        it "should call write_install_file" do
          @provisioner.should_receive(:write_install_file).and_return(true)          
        end
        it "should call process_install!" do
          @provisioner.should_receive(:process_install!).and_return(true)
        end
        after(:each) do
          hide_output {
            Provisioner.provision_master(@cloud, true)
          }
        end
      end
    end
  end
end