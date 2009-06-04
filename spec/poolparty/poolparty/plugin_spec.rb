require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Resources
require File.dirname(__FILE__) + '/test_plugins/webserver'

describe "Plugin" do
  describe "wrapped" do
    before(:each) do
      reset!
      cloud :app_for_plugin do
        apache_test do
          enable_php
          site("heady", {
            :document_root => "/root"
          })
        end
        apache_test do
          site("boob")
        end
      end
      @plugin = clouds[:app_for_plugin].apache_test
    end
    it "should not be empty" do
      clouds[:app_for_plugin].apache_test.class.should == ::PoolParty::Plugin::ApacheTest
    end
    it "should set loaded == true" do
      clouds[:app_for_plugin].apache_test.loaded.should == true
    end
    it "should have enable_php as a method" do
      ::PoolParty::Plugin::ApacheTest.new.respond_to?(:enable_php).should == true
    end
    it "should set enable_php" do
      @plugin.enable_php.should == true
    end
    it "should store the plugin in the clouds plugin_store" do
      clouds[:app_for_plugin].plugin_store[-2].class.should == @plugin.class
      clouds[:app_for_plugin].apache_test
      clouds[:app_for_plugin].plugin_store[-2].class.should == @plugin.class
    end
  end
end