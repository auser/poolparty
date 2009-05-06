require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/test_plugins/webserver'

describe "Plugin" do
  before(:each) do    
    @c = cloud :test_plugin_model_cloud do
      apachetest do                
        site("heady", {
          :document_root => "/root"
        })
      end
    end
  end
  describe "methods should include" do
    it "register_plugin(plugin)" do;WebServers.respond_to?(:register_plugin).should == true;end
  end
  describe "registered" do
    before(:each) do
      @plugin = @c.apachetest
    end
    describe "storage" do
      it "should be able to retrieve the plugin as a name" do
        @c.plugin("apachetest").should_not be_nil
      end
    end
    it "be of the class apachetesttestClass on the Kernel" do
      @plugin.class.should == Kernel::ApachetestClass
    end
    it "should store the regsitered plugins in an array" do
      @plugin.should_not be_nil
    end
    it "should have the plugin name as a method on the cloud " do
      @c.respond_to?(:apachetest).should == true
    end
    describe "methods" do
      it "should call the enable_php method when in the defininition of the cloud" do
        @plugin.respond_to?(:enable_php).should == true
      end
      it "should call php = true in the enable_php" do
        @plugin.php?.should == false
        @plugin.enable_php
        @plugin.php.should == true
      end
      it "should call the site method when in the defininition of the cloud" do
        @plugin.respond_to?(:site).should == true
      end
      it "should be able to call the plugin method site" do
        @plugin.should_receive(:virtual_host).with("hop", {:document_root => "/root"})
        @plugin.virtual_host("hop", {:document_root => "/root"})
      end
    end
  end
end