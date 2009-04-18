require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty

describe "Neighborhoods" do
  it "should have a constant: Neighborhoods" do
    Object.const_get("Neighborhoods").should == PoolParty::Neighborhoods
  end
  it "should set the schema when starting with a json string (or a hash)" do
    n = Neighborhoods.new({:instances => "b"})
    n.schema.nil?.should == false
    n.schema.class.should == PoolParty::Schema
  end
  it "should raise an error if a new Neighborhood is created with a nil parameter" do
    lambda {Neighborhoods.new}.should raise_error
  end
  it "should raise an error if there are no instances defined in the json" do
    lambda {Neighborhoods.new({:a => "b"})}.should raise_error
  end
  it "should not raise a fit if there ARE instances defined" do
    lambda{Neighborhoods.new({:instances => ["10.0.0.1"]})}.should_not raise_error
  end
  it "should return an instance ip when calling for the first instance" do
    n = Neighborhoods.new(sample_instances_list)
    n.instances.first.ip.should == "127.0.0.1"
    n.instances[1].ip.should == "127.0.0.2"
  end
  it "should return an instance ip when calling for the [0] instance" do
    n = Neighborhoods.new(sample_instances_list)
    n[0].ip.should == "127.0.0.1"
    n[1].ip.should == "127.0.0.2"
  end
  it "should be able to try to save the instances into a json file" do
    filepath = "/tmp/poolparty/neighborhood.json"
    ::File.should_receive(:open).with(filepath, "w").and_return true
    Neighborhoods.new(sample_instances_list).clump(filepath)
  end
  it "should be able to try to save the instances (class method) into a json file" do
    filepath = "/tmp/poolparty/neighborhood.json"
    ::File.should_receive(:open).with(filepath, "w").and_return true
    Neighborhoods.clump(sample_instances_list,filepath)
  end
  it "should load from the default properly with the first's instance's ip" do
    str = "[{\"instance_id\":\"master\",\"launching_time\":\"2009/03/26 01:06:18 -0700\",\"ip\":\"127.0.0.1\"},{\"instance_id\":\"node1\",\"launching_time\":\"2009/03/26 01:06:18 -0700\",\"ip\":\"127.0.0.2\"}]"
    ::File.should_receive(:file?).with("/etc/poolparty/neighborhood.json").and_return true
    ::File.stub!(:file?).and_return false
    
    str.stub!(:read).and_return str
    Neighborhoods.should_receive(:open).with("/etc/poolparty/neighborhood.json").and_return str
    
    n = Neighborhoods.load_default
    n[0].ip.should == "127.0.0.1"
  end
  context "load_default" do
    it "should try to look in the paths to see if the neighborhood file exists" do
      ::File.should_receive(:file?).with("/etc/poolparty/neighborhood.json").and_return true
      ::File.stub!(:file?).and_return false
      @file = sample_instances_list
      @file.stub!(:read).and_return @file
      Neighborhoods.should_receive(:open).with("/etc/poolparty/neighborhood.json").and_return @file
      n = Neighborhoods.load_default
      n.instances.first.ip.should == "127.0.0.1"
    end
  end
end