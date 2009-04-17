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
  context "calling add_optional_enabled_services" do
    setup do
      reset!
      @cloud = cloud :test_cloud_dsl_2
      @cloud.instance_eval "self.class.send :attr_reader, :brains"
      @cloud.instance_eval "def box;@brains = 'pepper';end"
    end

    should "send @cloud the method box after it's been enabled" do
      @cloud.enable :box
      @cloud.add_optional_enabled_services
      @cloud.brains.should == "pepper"
    end
    should "not call box if the method has not been enabled" do
      @cloud.add_optional_enabled_services
      @cloud.brains.should == nil
    end
    should "call haproxy when adding enabled serivces" do
      @cloud.enable :haproxy
      @cloud.add_optional_enabled_services
      klasses = @cloud.plugin_store.map {|a| a.class.to_s.split("::")[-1] }
      klasses.include?("HaproxyClass").should == true
    end
  end
  
  
end