require File.dirname(__FILE__) + '/../spec_helper'

include Remote

class TestRemoter
  include RemoterBase  
end

describe "RemoterBase" do
  describe "methods" do
    before(:each) do
      @tr = TestRemoter.new
    end
    %w(launch_new_instance! terminate_instance describe_instance instances_list).each do |method|
      eval <<-EOE
        it "should raise an exception if #{method} is not defined as a method" do
          lambda { @tr.#{method} }.should raise_error
        end
        it "should not raise an exception if #{method} is defined as a method" do
          lambda {
            @tr.instance_eval do
              def #{method}                
              end
            end
            @tr.#{method}
          }.should_not raise_error
        end
      EOE
    end
    describe "lists" do
      before(:each) do
        stub_list_of_instances_for(@tr)
      end
      it "should gather a list of the running instances" do
        @tr.list_of_running_instances.map {|a| a.name }.should == ["master", "node1"]
      end
      it "should be able to gather a list of the pending instances" do
        @tr.list_of_pending_instances.map {|a| a.name }.should == ["node3"]
      end
      it "should be able to gather a list of the terminating instances" do
        @tr.list_of_terminating_instances.map {|a| a.name }.should == []
      end
      it "should be able to gather a list of the non-terminated instances" do
        @tr.list_of_nonterminated_instances.map {|a| a.name }.should == ["master", "node1", "node3"]
      end
      it "should return a list of remote instances" do
        @tr.remote_instances_list.first.class.should == RemoteInstance
      end
      describe "by keypairs" do
        it "should be able to grab all the alist keypairs" do
          @tr.list_of_instances("fake_keypair").map {|a| a[:name] }.should == ["master", "node1", "node2", "node3"]
        end
        it "should be able to grab all the blist keypairs" do
          @tr.list_of_instances("blist").map {|a| a[:name] }.should == ["node4"]
        end
      end
    end
    
    describe "adding custom install tasks (like set_hostname, for example)" do
      before(:each) do
        @master = Object.new
        @master.stub!(:ip).and_return "192.68.0.1"
        @tr.stub!(:master).and_return @master
      end
      it "should have the method custom_install_tasks" do;@tr.respond_to?(:custom_install_tasks_for).should == true;end
      it "should have the method custom_configure_tasks" do;@tr.respond_to?(:custom_configure_tasks_for).should == true;end
    end
  end
end