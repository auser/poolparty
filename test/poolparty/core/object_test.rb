require "#{::File.dirname(__FILE__)}/../../test_helper"

class RunOnceClass
  attr_reader :apples, :count
  def initialize
    @count = 1
  end
  def run_me
    do_once do
      @count += 1
      @apples = "to apples"
    end
  end
end

class RunOnceTest < Test::Unit::TestCase
  def setup
    @ro = RunOnceClass.new
  end
  def test_apples_should_be_nil
    assert_nil @ro.apples
    assert_equal @ro.count, 1
  end
  def test_apples_should_not_be_nil_when_run
    @ro.run_me
    assert_equal @ro.apples, "to apples"
    assert_equal @ro.count, 2
  end
end
