require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Cloud
include PoolParty::Resources

class TestServiceClass
  plugin :test_service do
    def enable
      has_file(:name => "/etc/poolparty/lobos")
    end                  
  end
end

describe "Cloud" do
  before(:each) do
    reset_resources!
  end
  describe "wrapped" do
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
        @cloud1.using_remoter?.should_not == nil
      end
      it "should say the remoter_base is ec2 (by default)" do
        @cloud1.remote_base.should == PoolParty::Ec2
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
          access_key "access_key"
          cloud :apple do          
            access_key "cloud_access_key"
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
      it "should take the access_key option set from the cloud" do
        @c.access_key.should == "cloud_access_key"
      end
    end
    describe "block" do
      before(:each) do
        reset!
        @cloud = Cloud.new(:test, @pool) do
          # Inside cloud block
          keypair "fake_keypair"
        end
      end

      it "should be able to pull the pool from the cloud" do
        @cloud.parent == @pool
      end
      it "should have services in an array" do
        @cloud.services.class.should == Array
      end
      it "should have no services in the array when there are no services defined" do
        @cloud.services.size.should == 0
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
            stub_list_from_remote_for(@cloud)
            @cloud.instance_eval do
              has_file(:name => "/etc/httpd/http.conf") do
                content <<-EOE
                  hello my lady
                EOE
              end
              has_gempackage(:name => "poolparty")
              has_package(:name => "dummy")            
            end
          end
          it "should it should have the method build_manifest" do
            @cloud.respond_to?(:build_manifest).should == true
          end
          it "should have 3 resources" do
            @cloud.add_poolparty_base_requirements
            @cloud.number_of_resources.should > 2
          end
          it "should receive add_poolparty_base_requirements before building the manifest" do
            @cloud.should_receive(:add_poolparty_base_requirements).once
            @cloud.build_manifest
          end
          describe "add_poolparty_base_requirements" do
            before(:each) do
              reset!            
              @cloud.instance_eval do
                @heartbeat = nil
              end
              @hb = "heartbeat".class_constant.new(@cloud)
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
            describe "after adding" do
              before(:each) do
                stub_list_from_remote_for(@cloud)
                @cloud.add_poolparty_base_requirements              
              end
              it "should add resources onto the heartbeat class inside the cloud" do
                @cloud.services.size.should > 0
              end
              it "should store the class heartbeat" do              
                @cloud.services.map {|a| a.class}.include?("heartbeat".class_constant).should == true
              end
              it "should have an array of resources on the heartbeat" do
                @cloud.services.first.resources.class.should == Hash
              end
              describe "resources" do
                before(:each) do
                  @cloud8 = Cloud.new(:tester, @pool) do     
                    test_service             
                  end
                  @service = @cloud8.services.first
                  @files = @service.resource(:file)
                end
                it "should have a file resource" do
                  @files.first.nil?.should == false
                end
                it "should have an array of lines" do
                  @files.class.should == Array
                end
                it "should not be empty" do
                  @files.should_not be_empty
                end
              end
            end
          end
          describe "building" do
            before(:each) do            
              str = "master 192.168.0.1
              node1 192.168.0.2"
              @sample_instances_list = [{:ip => "192.168.0.1", :name => "master"}, {:ip => "192.168.0.2", :name => "node1"}]
              @ris = @sample_instances_list.map {|h| PoolParty::Remote::RemoteInstance.new(h, @cloud) }

              @manifest = @cloud.build_manifest
            end
            it "should return a string when calling build_manifest" do
              @manifest.class.should == String
            end
            it "should have a comment of # file in the manifest as described by the has_file" do
              @manifest.should =~ /file \{/
            end
            it "should have the comment of a package in the manifest" do
              @manifest.should =~ /package \{/
            end
            it "should have the comment for heartbeat in the manifest" do
              @manifest.should =~ /class heartbeat/            
            end
            it "should include the poolparty gem" do
              @manifest.should =~ /package \{/
            end
            it "should include custom functions" do
              @manifest.should =~ /define line\(\$file/

              File.open("test_manifest.pp", "w+") {|f| f << @manifest}
            end
            it "should include the hosts for all the listed local instances" do
              @manifest.should =~ /host \{\n\t\t"master":/
            end
          end
          describe "building with an existing manifest" do
            before(:each) do
              @file = "/etc/puppet/manifests/nodes/nodes.pp"
              @file.stub!(:read).and_return "nodes"
              ::FileTest.stub!(:file?).with("/etc/puppet/manifests/classes/poolparty.pp").and_return true
              @cloud.stub!(:open).with("/etc/puppet/manifests/classes/poolparty.pp").and_return @file
            end
            it "should not call resources_string_from_resources if the file /etc/puppet/manifests/nodes/nodes.pp exists" do
              @cloud.should_not_receive(:add_poolparty_base_requirements)
              @cloud.build_manifest
            end
            it "should build from the existing file" do
              @cloud.build_manifest.should == "nodes"
            end
          end
        end
      end

      describe "instances" do
        before(:each) do
          @cloud3 = cloud :pop do;keypair "fake_keypair";end
          stub_list_from_remote_for(@cloud3)
        end
        it "should respond to the method master" do
          @cloud3.respond_to?(:master).should == true
        end
        it "should return a master that is not nil" do
          @cloud3.master.should_not be_nil
        end
      end

      describe "provisioning" do
        it "should be able to tell that it is provisioning when setting the provisioning flag" do
          @cloud.provisioning?.should == false
        end
        it "should be able to set it as provisioning" do
          @cloud.provisioning_in_progress
          @cloud.provisioning?.should == true
        end
        it "should be able to unset itself as provisioning" do
          @cloud.provisioning_in_progress
          @cloud.provisioning_complete
          @cloud.provisioning?.should == false
        end
      end
    end
  end
end