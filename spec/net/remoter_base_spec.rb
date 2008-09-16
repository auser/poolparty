require File.dirname(__FILE__) + '/../spec_helper'

include Remote

class TestRemoter < RemoterBase  
end

describe "RemoterBase" do
  describe "methods" do
    before(:each) do
      @tr = TestRemoter.new
    end
    %w(launch_new_instance! terminate_instance describe_instance instances_list).each do |method|
      eval <<-EOE
        it "should raise an exception if #{method} is not defined as a method" do
          lambda { @tr.#{method} }.should raise_error
        end
        it "should not raise an exception if #{method} is defined as a method" do
          lambda {
            @tr.instance_eval do
              def #{method}                
              end
            end
            @tr.#{method}
          }.should_not raise_error
        end
      EOE
    end
  end
end