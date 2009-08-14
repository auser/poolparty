require "#{File.dirname(__FILE__)}/../../test_helper"

class MonitorTest < Test::Unit::TestCase
  
  def setup
    @mon = PoolParty::Monitor.new(:cpu) do |c|
      vote_for(:expand) if c > 0.8
      configure if c < 0.1
    end
  end
  
  def test_monitor_initialize    
    assert_equal @mon.name, :cpu
    assert_equal Proc, @mon.monitor_block.class
  end
  
  def test_monitor_run_and_method_rettrieval    
    assert_equal({:vote_for => [:expand]}, @mon.run(0.9))
    assert_equal({}, @mon.run(0.3))
    assert_equal({:configure => []}, @mon.run(0.04))
  end
  
  def test_should_explode_if_no_block_is_given
    PoolParty::PoolPartyError.create("MonitorDefinitionError")
    assert_raises MonitorDefinitionError do
      PoolParty::Monitor.new :memory
    end
  end
  
end