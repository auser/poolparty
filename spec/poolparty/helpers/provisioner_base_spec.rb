require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/../../../lib/poolparty/helpers/provisioner_base'

include Provisioner

describe "ProvisionerBase" do
  before(:each) do
    @cloud = cloud :app do; end
    stub_list_from_remote_for(@cloud)
  end
  it "should respond to the class method install" do
    ProvisionerBase.respond_to?(:install).should == true
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
      @tp = TestProvisioner.new(@cloud)
    end
    it "should have tasks on the provisioner" do
      ProvisionerBase.new(@cloud).tasks.class.should == Array
    end
    it "should have empty tasks on the provisioner" do
      ProvisionerBase.new(@cloud).tasks.should be_empty
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
    ProvisionerBase.new(@cloud).installer_for("ubuntu").should == "apt-get install"
  end
  it "should be able to fetch the fedora installer with the helper method installer" do
    ProvisionerBase.new(@cloud).installer_for("fedora").should == "yum install"
  end
  describe "install_string" do
    before(:each) do
      class Provisioner::BTestProvisioner < Provisioner::ProvisionerBase
        def tasks
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
      end
      @provisioner = BTestProvisioner.new(@cloud)
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
    after do
      @provisioner.install_string
    end
  end
end