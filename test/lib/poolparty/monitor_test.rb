require "#{File.dirname(__FILE__)}/../../test_helper"

class MonitorTest < Test::Unit::TestCase
  
  def setup
    @mon = PoolParty::Monitor.new("cpu-idle") do |c|
      vote_for(:expand) if c > 0.8
      configure if c < 0.1
    end
  end
  
  def test_monitor_initialize    
    assert_equal @mon.name, :'cpu-idle'
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
  
  def test_formatting_input
    mon = PoolParty::Monitor.new("memory-used") {|c| long if c.length > 2}
    mon.format(:to_s)
    assert_equal({:long => []}, mon.run("hellllllllooooo world"))
    
    mon = PoolParty::Monitor.new("memory-used") do |c| 
      long if c.length > 2
      short if c.length < 2
    end
    mon.format(:to_a)
    assert_equal({:long => []}, mon.run(%w(1 2 3 4)))
    assert_equal({:short => []}, mon.run(%w(1)))
    
    mon = PoolParty::Monitor.new("memory-used") do |saying, to| 
      if saying == "hello"
        hello
      else
        goodbye
      end
    end
    
    mon.format {|d|return *d.split(",")}

    assert_equal({:hello => []}, mon.run("hello, world"))
    assert_equal({:short => []}, mon.run("good day"))
  end
  
end