require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Monitors

def vm_stat_str
<<-EOE
Mach Virtual Memory Statistics: (page size of 4096 bytes)
Pages free:                    67993.
Pages active:                 256445.
Pages inactive:               124392.
Pages wired down:              73762.
"Translation faults":     1280661664.
Pages copy-on-write:        52365921.
Pages zero filled:         722428360.
Pages reactivated:            441434.
Pageins:                     1120320.
Pageouts:                     285425.
Object cache: 11159288 hits of 21753237 lookups (51% hit rate)
EOE
end

describe "MemoryMonitor" do
  before(:each) do
    @mon = MemoryMonitor.new
    MemoryMonitor.stub!(:new).and_return @mon
  end
  it "should have the singleton method run defined" do
    MemoryMonitor.respond_to?(:run).should == true
  end
  it "should call the method new" do
    MemoryMonitor.should_receive(:new).and_return @mon
    MemoryMonitor.run
  end
  describe "calling" do
    it "should call uname with %x" do
      @mon.should_receive(:`).with("uname").and_return "Darwin"
      @mon.stub!(:`).and_return ""
    end
    it "should call vm_stat with when the uname returns Darwin" do
      @mon.should_receive(:`).with("uname").and_return "Darwin"
      @mon.should_receive(:`).with("vm_stat").and_return vm_stat_str
    end
    after(:each) do
      @mon.run
    end
  end
end