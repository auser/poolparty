require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestCloudDsl < Test::Unit::TestCase
  context "enabling and disabling" do
    setup do
      reset!
      @cloud = cloud :test_cloud_dsl
    end    
    should "not have an option set for hyper_drive" do
      @cloud.dsl_options[:hyper_drive].should == nil
    end

    should "set the option to :enabled when enabled" do
      @cloud.enable :hyper_drive
      @cloud.dsl_options[:hyper_drive].should == :enabled
    end

    should "set the option to disabled when disabled" do
      @cloud.disable :hyper_drive
      @cloud.dsl_options[:hyper_drive].should == :disabled
    end
    
    should "be able to check if they are enabled" do
      @cloud.enable :hyper_drive
      @cloud.enabled?(:hyper_drive).should == true
    end
    should "be able to check that they are disabled" do
      @cloud.disable :hyper_drive
      @cloud.enabled?(:hyper_drive).should == false
    end
  end
  
end