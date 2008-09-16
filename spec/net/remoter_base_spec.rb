require File.dirname(__FILE__) + '/../spec_helper'

include Remote

class TestRemoter < RemoterBase  
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
        @a1={:instance_id => "i-a1", :ip => "127.0.0.1", :status => "running", :launching_time => 10.minutes.ago, :keypair => "alist"}
        @a2={:instance_id => "i-a2", :ip => "127.0.0.3", :status => "running", :launching_time => 2.hours.ago, :keypair => "alist"}
        @a3={:instance_id => "i-a3", :ip => "127.0.0.3", :status => "terminated", :launching_time => 2.hours.ago, :keypair => "alist"}
        @a4={:instance_id => "i-a4", :ip => "127.0.0.4", :status => "pending", :launching_time => 2.hours.ago, :keypair => "alist"}

        @b1={:instance_id => "i-b1", :ip => "127.0.0.2", :status => "shutting down", :launching_time => 55.minutes.ago, :keypair => "blist"}
        @c1={:instance_id => "i-c1", :ip => "127.0.0.4", :status => "pending", :launching_time => 2.days.ago, :keypair => "clist"}
        @tr.stub!(:instances_list).and_return [@a1, @a2, @a3, @a4, @b1, @c1]
      end
      it "should gather a list of the running instances" do
        @tr.list_of_running_instances.map {|a| a[:instance_id] }.should == ["i-a1", "i-a2"]
      end
      it "should be able to gather a list of the pending instances" do
        @tr.list_of_pending_instances.map {|a| a[:instance_id] }.should == ["i-a4", "i-c1"]
      end
      it "should be able to gather a list of the terminating instances" do
        @tr.list_of_terminating_instances.map {|a| a[:instance_id] }.should == ["i-b1"]
      end
      it "should be able to gather a list of the non-terminated instances" do
        @tr.list_of_nonterminated_instances.map {|a| a[:instance_id] }.should == ["i-a1", "i-a2", "i-a4", "i-b1", "i-c1"]
      end
      describe "by keypairs" do
        it "should be able to grab all the alist keypairs" do
          @tr.list_of_instances("alist").map {|a| a[:instance_id] }.should == ["i-a1", "i-a2", "i-a3", "i-a4"]
        end
      end
    end
    
  end
end