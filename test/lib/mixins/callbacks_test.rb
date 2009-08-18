require "#{::File.dirname(__FILE__)}/../../test_helper"

class CallbacksTestClass
  attr_accessor :configured, :current_callback
  include Callbacks

  callback_block do |inst, callback|
    inst.current_callback = callback
  end
  
  def before_configure
    @configured = true
  end
end

class TestCallbacks < Test::Unit::TestCase
  context "methods" do
    setup do
      @tc = CallbacksTestClass.new
    end

    should "call the callback on the class when calling call_(\w+)_callbacks method" do
      assert_nil @tc.configured
      @tc.callback :before_configure
      assert @tc.configured
    end
    should "not explode when the method does not exist on the call_(\w+)_callbacks method" do
      lambda {@tc.callback :after_boxed}.should_not raise_error
    end
    
    should "call the callback_block with the callback call" do
      assert_nil @tc.current_callback
      @tc.callback :before_configure
      assert_equal :before_configure, @tc.current_callback
    end
    
    should "have the callbacks available in an array" do
      @tc.callback :before_configure
      assert_equal [:before_configure], @tc.callbacks
    end
    
    should "call the actual callback" do
      @tc.callback :before_configure
      assert @tc.configured
    end
  end
  
end