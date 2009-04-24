require "#{::File.dirname(__FILE__)}/../../test_helper"

class CallbacksTestClass
  attr_reader :configured
  include PoolParty::Callbacks
  
  additional_callbacks ["after_boxes"]
  
  def before_configure
    @configured = true
  end
end

class TestCallbacks < Test::Unit::TestCase
  context "methods" do
    setup do
      @tc = CallbacksTestClass.new
      @tc.after_create
    end

    should "have the 4 basic callbacks" do
      assert @tc.respond_to?(:call_before_bootstrap_callbacks)
      assert @tc.respond_to?(:call_after_bootstrap_callbacks)
      assert @tc.respond_to?(:call_before_configure_callbacks)
      assert @tc.respond_to?(:call_after_configure_callbacks)
    end
    should "have an addiitonal callback caller method" do
      assert @tc.respond_to?(:call_after_boxes_callbacks)
    end
    should "call the callback on the class when calling call_(\w+)_callbacks method" do
      assert_nil @tc.configured
      @tc.call_before_configure_callbacks
      assert @tc.configured
    end
    should "not explode when the method does not exist on the call_(\w+)_callbacks method" do
      lambda {@tc.call_after_boxes_callbacks}.should_not raise_error
    end
  end
  
end