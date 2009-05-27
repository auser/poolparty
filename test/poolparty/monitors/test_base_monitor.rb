require "#{::File.dirname(__FILE__)}/../../test_helper"

class BaseMonitorTest < Test::Unit::TestCase
  context "Available Monitors" do
    should "start out with available monitors" do
      assert Monitors.available_monitors.size > 5
      assert Monitors.available_monitors.include? Monitors::Load
    end
    
    should "have available_monitors" do
      class ExtraFoo < ::Monitors::BaseMonitor; end
      assert Monitors.available_monitors.include? ExtraFoo
    end

  end
 
end