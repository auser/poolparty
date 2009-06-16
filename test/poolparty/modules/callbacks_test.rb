require "#{File.dirname(__FILE__)}/../../test_helper"

class CallbackTestClass
  include PoolParty::Callbacks
  attr_reader :var
  attr_accessor :last_callback
  
  callback_block do |instance, time|
    instance.last_callback = time
  end
  
  def after_load(v)
    @var = v
  end
end

class CallbacksTest < Test::Unit::TestCase
  context "callbacks" do
    setup do
      @cClass = CallbackTestClass.new
    end

    should "call the callback method on the object" do
      assert_equal @cClass.var, nil
      @cClass.callback :after_load, "a"
      assert_equal @cClass.var, "a"
      assert_equal :after_load, @cClass.last_callback
    end
    
    should "not call a method if it isn't on the object" do
      assert_nothing_raised do 
        @cClass.callback :after_sleep
      end
      assert_equal :after_sleep, @cClass.last_callback
    end
    
    should "have a list of the callbacks available" do
      assert_equal @cClass.callbacks.class, Array      
    end
    
  end
  
end