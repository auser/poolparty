require File.join(File.dirname(__FILE__), '../../spec_helper')
require File.dirname(__FILE__) + '/ec2_mocks_and_stubs.rb'

include PoolParty::Remote


describe "Ec2RemoteInstance" do
  before(:each) do
    @cloud = TestCloud.new :test_remoter_base_cloud
    @tec2 = TestEC2Class.new(@cloud.dsl_options)
    @ec2 = TestEc2RemoteInstance.new( @tec2.dsl_options.merge(:name => "node3", :status => "pending", :ip => "192.168.0.3"))
  end
  it "should have the name set in the options" do
    @ec2.respond_to?(:name).should == true
    @ec2.dsl_options.keys.include?(:name).should == true
    @ec2.name.should == 'node3'
  end
  it "should have access to parent cloud options" do
    @ec2.instance_type.should == 'm1.small'
  end
  it "should have instance description hash mapped to methods" do
    # puts  "<ol>"
    #   @ec2.my_cloud.describe_instances.each{|i| puts "<li>#{i.inspect}</li>"}
    # puts " </ol>"
    @ec2.status.should == 'pending'
    @ec2.name.should  == 'node3'
    @ec2.ip.should == '192.168.0.3'
  end
end

describe "Remote Instance" do
  before(:each) do
    @valid_hash = {:ip => "127.0.0.1", :name => "master"}
  end

  it "should create a remote instance with a Hash" do
    @ec2 = TestEc2RemoteInstance.new(@valid_hash)
    @ec2.valid?.should == true
  end
  it "should not be valid if there is no ip associated" do
    @ec2 = TestEc2RemoteInstance.new({:ip => nil})
    @ec2.valid?.should == false
  end
  it "should not be valid if there is no name associated" do
    @ec2 = TestEc2RemoteInstance.new(@valid_hash.merge({:name => nil}))
    @ec2.valid?.should == false
  end
  describe "status" do
    it "should say it is running when the status == running" do
      TestEc2RemoteInstance.new(@valid_hash.merge(:status => "running")).running?.should == true
    end
    it "should say it is terminating when the status == shutting down" do
      TestEc2RemoteInstance.new(@valid_hash.merge(:name => "node4", :status => "shutting")).terminating?.should == true
    end
    it "should say it is terminated when the status == terminated" do
      TestEc2RemoteInstance.new(@valid_hash.merge(:status => "terminated")).terminated?.should == true
    end
    it "should not say it is running when it is pending" do
      TestEc2RemoteInstance.new(@valid_hash.merge(:status => "pending") ).running?.should == false
    end
  end
  describe "methods" do
    before(:each) do
      @ec2 = TestEc2RemoteInstance.new(@valid_hash.merge(:status => "running"))
    end
    it "should give the elapsed time" do
      @ec2.stub!(:launching_time).and_return(30.minutes.ago)
      @ec2.elapsed_runtime.should be >= 1800
    end
    it "should say that it is responding? if responding is not nil" do
      @ec2.running?.should == true
    end
  end
end
