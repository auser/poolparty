require "#{::File.dirname(__FILE__)}/../../test_helper"

::Monitors
class ExtraFoo < Monitors::BaseMonitor
end

class BaseMonitorTest < Test::Unit::TestCase
  context "Available Monitors" do
    
    should "have available_monitors" do
      assert !Monitors::BaseMonitor.available_monitors.nil?
      assert Monitors::BaseMonitor.available_monitors.include? ExtraFoo
    end

  end
 
end