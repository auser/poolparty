require File.dirname(__FILE__) + '/../spec_helper'

describe "haproxy base package" do
  it "should have the haproxy package defined" do
    lambda {PoolpartyBaseHaproxyClass}.should_not raise_error
  end
end