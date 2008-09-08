require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/../../lib/poolparty/helpers/provisioner'

include Provisioner

describe "ProvisionerBase" do
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
      @tp = TestProvisioner.new
    end
    it "should have tasks on the provisioner" do
      ProvisionerBase.new.tasks.class.should == Array
    end
    it "should have empty tasks on the provisioner" do
      ProvisionerBase.new.tasks.should be_empty
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
    ProvisionerBase.installer_for("ubuntu").should == "apt-get"
  end
end