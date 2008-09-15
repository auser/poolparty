require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/../../lib/poolparty/helpers/display'

include Display

describe "Display" do
  it "should respond to pool_describe" do
    self.respond_to?(:pool_describe).should == true
  end
end