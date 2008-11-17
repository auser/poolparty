require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/test_plugins/webserver'

include PoolParty::Resources

describe "Plugin" do
  before(:each) do
    reset_resources!
  end
  describe "wrapped" do
    before(:each) do
      reset!
      pool :poolpartyrb do      
        cloud :app do
          apache do
            enable_php
            site("heady", {
              :document_root => "/root"
            })
          end
        end      
      end
      @p = pool :poolpartyrb
      @c = @p.cloud(:app)
    end

    it "should allow access to the pool on the cloud" do
      @c.parent.should == @p
    end
    describe "instance" do
      before(:each) do        
        @plugin = @c.apache
      end
      it "should not be empty" do
        @plugin.class.should == PoolPartyApacheClass
      end
      it "should have access to the cloud's container" do
        @plugin.parent.should == @c
      end
      it "should have enable_php as a method" do
        @plugin.respond_to?(:enable_php).should == true
      end
      describe "after eval'ing" do
        before(:each) do
          @plugin.instance_eval do
            enable_php
            has_gempackage(:name => "aska")
          end
        end
        it "should call enable_php on the class" do
          @plugin.php.should == true
        end
        it "should have resources attached to it" do
          @plugin.resources.class.should == Hash
        end
        it "should have an array of gem resources" do
          @plugin.resource(:gempackage).class.should == Array
        end
        it "should have 1 gem in the resources defined" do
          @plugin.resource(:gempackage).size.should == 1
        end
        it "should have the gem named aska in the gem resource" do
          @plugin.get_resource(:gempackage, "aska").name.should == "aska"
        end
        it "should have its resources visible to its parent" do
          @plugin.parent = @c
          @c.resources
        end
      end
      describe "before eval'ing" do
        before(:each) do
          reset!
          @plugin = "apache".class_constant.new(@c)
        end
        it "should call has_line_in_file" do
          @plugin.should_receive(:php).and_return true
        end
        it "should call site" do
          @plugin.should_receive(:site).with("frank").and_return true
        end
        after do
          @plugin.instance_eval do
            enable_php
            site("frank")
          end
        end
      end
    end
    describe "DSL" do
      it "should have a cloud method from within the plugin" do
        @plugin.respond_to?(:cloud).should == true
      end
      it "should get a hold of the containing cloud" do
        @plugin.cloud.should == @c
      end
    end
  end
end