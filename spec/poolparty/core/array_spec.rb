require File.dirname(__FILE__) + '/../spec_helper'

describe "Array" do
  before(:each) do
    @array = ["run this", "run that"]
  end
  describe "runnable" do
    it "should be able to produced a runnable string" do;@array.runnable.should == 'run this  &&  run that';end
    it "should be turn into a string" do;@array.runnable.class.should == String;end
  end
  describe "nice_runnable" do
    before(:each) do
      @array << ""
      @array << "peabody"      
    end
    it "should reject any empty elements out of the array" do
      (@array << [""]).nice_runnable.should == "run this \n run that \n peabody"
    end
  end
  describe "to_os" do
    it "should be able to turn itself to an open struct" do; @array.first.should_receive(:to_os);end        
    after do
      @array.to_os
    end
  end
  require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

  describe "select_with_hash" do
    before(:all) do
      @remote_instances_list =[
        {:status=>'running', :ip=>'10'}, 
        {:status => 'pending', :ip=>'not.assigned'}, 
        {:status=>'running', :ip=>'192', :bogus=>nil} 
      ]
    end
    it "should return the selected array" do
        @remote_instances_list.select_with_hash( {:status => 'running'}).size.should == 2
        @remote_instances_list.select_with_hash( {:ip => 'not.assigned'}).should == [{:status => 'pending', :ip=>'not.assigned'},]
        @remote_instances_list.select_with_hash( {:bogus => nil}).size.should == 1
        @remote_instances_list.select_with_hash().size.should == 0
    end
    it "should not raise an error if element does not have key" do
      @remote_instances_list.select_with_hash( {:snot => 'runny'}).size.should == 0
    end
  end
end