require File.dirname(__FILE__) + '/../spec_helper'

class TestMonitorClass
  include PoolParty::Monitors
  include Configurable
  def default_options
    {}
  end
end

describe "Monitors" do
  it "should have a list of available monitors" do
    PoolParty::Monitors.available_monitors.empty?.should == false
  end
  it "should register a module and append it to the available monitors" do
    size = PoolParty::Monitors.available_monitors.size
    PoolParty::Monitors.register_monitor :fake
    size.should == PoolParty::Monitors.available_monitors.size - 1
  end
  it "should define the mehtod fake when register_monitor :fake" do
    PoolParty::Monitors.register_monitor :fake
    TestMonitorClass.new.respond_to?(:fake).should == true
  end
  it "should call the Messenger with messenger_send!('get_load fake')" do
    PoolParty::Messenger.should_receive(:messenger_send!).with("get_load fake").and_return true
    TestMonitorClass.new.fake
  end
  it "should have the cpu method on the class" do
    PoolParty::Messenger.should_receive(:messenger_send!).with("get_load cpu").and_return true
    TestMonitorClass.new.cpu
  end
  describe "expansions" do
    before(:each) do
      @tmc = TestMonitorClass.new
      @tmc.instance_eval do
        expand_when "cpu > 95", "memory > 90"
        contract_when "cpu < 15", "memory < 10"
      end
    end
    it "should have the method expand_when as a class method" do
      @tmc.respond_to?(:expand_when).should == true
    end
    it "should have expansions as a collection of rules on the object" do
      @tmc.expand_when.class.should == Aska::Rules
    end
    it "should return the expand_when as a set of rules" do
      @tmc.expand_when.class.should == Aska::Rules
    end
    it "should have the expansion in the array on the instance" do
      @tmc.expand_when.first.should == {"cpu" => [">", "95"]}
    end
    it "should have the memory expansion in the array on the instance" do
      @tmc.expand_when[1].should == {"memory" => [">", "90"]}
    end
    it "should have 2 expansions" do
      @tmc.expand_when.size.should == 2
    end
    it "should give us the method cpu for free" do
      @tmc.respond_to?(:cpu).should == true
    end
    describe "when memory is over the limit" do
      before(:each) do
        @tmc.stub!(:memory).and_return 99
      end
      it "should say it should not expand if the cpu is short of the limit" do
        @tmc.stub!(:cpu).and_return 43
        @tmc.valid_rules?(:expand_when).should == false
      end
      it "should say that it has the valid expansion rules when the cpu is higher than the limit" do
        @tmc.stub!(:cpu).and_return 98
        @tmc.valid_rules?(:expand_when).should == true
      end
    end
    describe "when cpu is over the limit" do
      before(:each) do
        @tmc.stub!(:cpu).and_return 98
      end
      it "should say it should not expand if the cpu is short of the limit" do
        @tmc.stub!(:memory).and_return 43
        @tmc.valid_rules?(:expand_when).should == false
      end
      it "should say that it has the valid expansion rules when the cpu is higher than the limit" do
        @tmc.stub!(:memory).and_return 98
        @tmc.valid_rules?(:expand_when).should == true
      end
    end
    describe "when neither is over the limits" do
      it "should not expand" do
        @tmc.valid_rules?(:expand_when).should == false
      end
    end
    it "should be able to turn them into a string" do
      @tmc.expand_when.to_s.should == "'cpu > 95', 'memory > 90'"
    end
    it "should be able to turn them into a string with expand_when" do
      @tmc.expand_when.to_s.should == "'cpu > 95', 'memory > 90'"
    end
  end
end
describe "BaseMonitor" do
  it "should have the singleton method run defined" do
    PoolParty::Monitors::BaseMonitor.respond_to?(:run).should == true
  end
end