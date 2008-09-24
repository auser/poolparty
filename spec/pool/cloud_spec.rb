require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Cloud

describe "Cloud" do
  before(:each) do
    @obj = Object.new
    @pool = pool :just_pool do; end
  end
  it "should respond to the pool method outside the block" do
    @obj.respond_to?(:cloud).should == true
  end
  describe "global" do
    before(:each) do
      @cloud1 = cloud :pop do;end
    end
    it "should store the cloud in the global list of clouds" do    
      @obj.clouds.has_key?(:pop).should == true
    end
    it "should store the cloud" do
      @obj.cloud(:pop).should == @cloud1
    end
    it "should have set the using base on intantiation to ec2" do
      @cloud1.using_remoter?.should == :ec2
    end
  end
  it "should return the cloud if the cloud key is already in the clouds list" do
    @cld = cloud :pop do;end
    @pool.cloud(:pop).should == @cld
  end
  describe "options" do
    before(:each) do
      reset!
      @p = pool :options do
        minimum_instances 100
        cloud :apple do          
        end
      end
      @c = @p.cloud(:apple)
    end
    it "should be able to grab the cloud from the pool" do
      @c.should == @p.cloud(:apple)
    end
    it "should take the options set on the pool" do
      @p.minimum_instances.should == 100
    end
    it "should take the options set from the pool" do
      @c.minimum_instances.should == 100
    end
  end
  describe "block" do
    before(:each) do
      reset!
      @cloud = Cloud.new(:test, @pool) do
        # Inside cloud block
      end
    end
    
    it "should be able to pull the pool from the cloud" do
      @cloud.parent == @pool
    end
    it "should respond to a configure method" do
      @cloud.respond_to?(:configure).should == true
    end
    describe "configuration" do
      before(:each) do
        reset!
        @cloud2 = Cloud.new(:test, @pool) do
          minimum_instances 1
          maximum_instances 2
        end
      end
      it "should be able to se the minimum_instances without the var" do
        @cloud2.minimum_instances.should == 1
      end
      it "should be able to se the maximum_instances with the =" do
        @cloud2.maximum_instances.should == 2
      end
    end
    describe "options" do
      it "should set the minimum_instances to 2" do
        @cloud.minimum_instances.should == 2
      end
      it "should set the maximum_instances to 4" do
        @cloud.maximum_instances.should == 4
      end
      it "should be able to set the minimum instances" do
        @cloud.minimum_instances 3
        @cloud.minimum_instances.should == 3
      end
      it "should be able to take a hash from configure and convert it to the options" do
        @cloud.configure( {:minimum_instances => 1, :maximum_instances => 10, :keypair => "friend"} )
        @cloud.keypair.should == "friend"
      end
      describe "minimum_instances/maximum_instances as a range" do
        before(:each) do
          reset!
          @pool = pool :just_pool do
            cloud :app do
              instances 8..15
            end
          end
          @cloud = @pool.cloud(:app)
        end
        it "should set the minimum based on the range" do
          @cloud.minimum_instances.should == 8
        end
        it "should set the maximum based on the range set by instances" do
          @cloud.maximum_instances.should == 15
        end
      end
      describe "keypair" do
        before(:each) do
          reset!
        end
        it "should be able to define a keypair in the cloud" do
          @c = cloud :app do
            keypair "hotdog"
          end
          @c.keypair.should == "hotdog"
        end
        it "should take the pool parent's keypair if it's defined on the pool" do
          pool :pool do
            keypair "ney"
            cloud :app do
            end
            cloud :group do
            end
          end
          pool(:pool).cloud(:app).keypair.should == "ney"
          pool(:pool).cloud(:group).keypair.should == "ney"
        end
        it "should generate a keypair based on the cloud name if none is defined" do
          pool :pool do
            cloud :app do
            end
            cloud :nickes do
            end
          end
          pool(:pool).cloud(:app).keypair.should == "pool_app"
          pool(:pool).cloud(:nickes).keypair.should == "pool_nickes"
        end
      end
      describe "Manifest" do
        before(:each) do
          reset!
          @cloud.instance_eval do
            has_file(:name => "/etc/httpd/http.conf") do
              content <<-EOE
                hello my lady
              EOE
            end
            has_gem(:name => "poolparty")
            has_package(:name => "dummy")
          end
        end
        it "should it should have the method build_manifest" do
          @cloud.respond_to?(:build_manifest).should == true
        end
        it "should have 3 resources" do
          @cloud.add_poolparty_base_requirements
          @cloud.number_of_resources.should == 3
        end
        it "should receive add_poolparty_base_requirements before building the manifest" do
          @cloud.should_receive(:add_poolparty_base_requirements).once
          @cloud.build_manifest
        end
        describe "add_poolparty_base_requirements" do
          before(:each) do
            reset!
            @hb = "heartbeat".class_constant.new
            @cloud.instance_eval do
              @heartbeat = nil
            end
          end
          it "should call initialize on heartbeat (in add_poolparty_base_requirements)" do
            @hb.class.should_receive(:new).and_return true
            @cloud.add_poolparty_base_requirements
          end
          it "should call heartbeat on the cloud" do
            @cloud.should_receive(:heartbeat).and_return true
            @cloud.add_poolparty_base_requirements
          end
          it "should call Hearbeat.new" do
            "heartbeat".class_constant.should_receive(:new).and_return @hb
            @cloud.add_poolparty_base_requirements            
          end
          it "should call enable on the plugin call" do
            @hb = "heartbeat".class_constant
            "heartbeat".class_constant.stub!(:new).and_return @hb
            
            @cloud.add_poolparty_base_requirements
            @cloud.heartbeat.should == @hb
          end
          it "should call has_line when calling heartbeat" do
            @cloud.should_receive(:has_line_in_file).at_least(1)
            @cloud.add_poolparty_base_requirements
          end
          it "should call has_line... when calling heartbeat which calls the call_function method" do
            PoolParty::Resources::CallFunction.should_receive(:new).and_return "bunk"
            @cloud.add_poolparty_base_requirements
          end
        end
        describe "building" do
          before(:each) do
            @manifest = @cloud.build_manifest
          end
          it "should return a string when calling build_manifest" do
            @manifest.class.should == String          
          end
          it "should have a comment of # file in the manifest as described by the has_file" do
            @manifest.should =~ /# files/
          end
          it "should have the comment of a package in the manifest" do
            @manifest.should =~ /# packages/
            puts @manifest
          end
        end
      end
    end
    
  end
end