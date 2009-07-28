require "#{::File.dirname(__FILE__)}/../../test_helper"

class DelayedTestClass
  include Delayed
  
  attr_reader :name
  def initialize(n)
    @name = n
  end
  def say(msg)
    "me: #{msg}"
  end
  def respond(msg)
    delayed_action do
      "#{name}: #{msg}"
    end
  end
  
  def after_all_loaded
    run_after_loaded do |k,v|
      puts "hi: #{k}"
    end
  end
end

class DelayedTest < Test::Unit::TestCase
  
  def setup
    @inst = DelayedTestClass.new("Amy")
  end
  
  def test_loaded_bang
    assert_equal false, @inst.loaded
    @inst.loaded!
    assert_equal true, @inst.loaded
  end
  
  def test_delayed_action_output
    assert_equal({}, DelayedTestClass.delayed_calls)
    assert_equal "me: hello", @inst.say("hello")
    assert_equal DelayedProc, @inst.respond("hi back").class
  end
  
  def test_running_delayed_actions
    @inst.loaded!
    @inst.after_all_loaded
    assert_equal "Amy: hi back", @inst.respond("hi back")
  end
  
end