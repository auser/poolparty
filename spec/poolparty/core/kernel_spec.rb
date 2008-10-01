require File.dirname(__FILE__) + '/../spec_helper'

describe "Kernel extensions" do
  before(:each) do
    @obj = Object.new
  end
  it "should eval the string into time" do
    @obj.should_receive(:sleep).once.with(10.seconds).and_return true
    @obj.wait "10.seconds"
  end
end
describe "Object extensions" do
  before(:each) do
    @klass = Object.new
    @klass.instance_eval <<-EOE
      def hello
        puts "hello"
      end
    EOE
  end
  it "should be able to get a list of the defined methods on the object" do
    @klass.my_methods.should == ["hello"]
  end
end