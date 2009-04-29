require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Resources
require File.dirname(__FILE__) + '/test_plugins/webserver'

describe "Plugin" do
  describe "wrapped" do
    before(:each) do
      reset!
      cloud :app_for_plugin do
        apachetest do
          enable_php
          site("heady", {
            :document_root => "/root"
          })
        end
        apachetest do
          site("boob")
        end
      end
      @plugin = clouds[:app_for_plugin].apachetest
    end
    it "should not be empty" do
      clouds[:app_for_plugin].apachetest.class.should == ApachetestClass
    end
    it "should set loaded == true" do
      clouds[:app_for_plugin].apachetest.loaded.should == true
    end
    it "should have enable_php as a method" do
      ApachetestClass.new.respond_to?(:enable_php).should == true
    end
    it "should set enable_php" do
      @plugin.enable_php.should == true
    end
    it "should store the plugin in the clouds plugin_store" do
      clouds[:app_for_plugin].plugin_store[-2].class.should == @plugin.class
      clouds[:app_for_plugin].apachetest
      clouds[:app_for_plugin].plugin_store[-2].class.should == @plugin.class
    end
  end
end