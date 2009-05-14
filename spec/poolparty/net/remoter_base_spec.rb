require File.dirname(__FILE__) + '/../spec_helper'

class TestRemoteClass < PoolParty::Remote::RemoteInstance
  include CloudResourcer
  include PoolParty::Remote
end

describe "RemoterBase" do
  before(:each) do
    @tr = TestRemoteClass.new
  end
  %w(launch_new_instance! terminate_instance describe_instance instances_list).each do |method|
    eval <<-EOE
        it "should raise an exception if #{method} is not defined as a method" do
          # pending # Weird .should raise_error
          lambda { @tr.class.#{method} }.should raise_error
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
    
