require File.dirname(__FILE__) + '/bin_spec_helper'
require "poolparty"

class PuppetResolverSpec
  plugin :apache do
  end
end

describe "Server list active binary" do
  describe "with real cloud" do
    before(:each) do
      Kernel::HaproxyClass.stub!(:new).and_return nil
      $pool_specfile = __FILE__
      @p = PoolParty::Pool::Pool.load_from_file "#{::File.dirname(__FILE__)}/fixtures/bin_cloud_for_test.rb"
      @pool_hash = @p.to_properties_hash
    end
    it "should have the name on the schema" do
      @pool_hash.options.name.should == :binary_testing_cloud
    end
    it "should have the remoter base available" do
      @pool_hash.options[:remote_base].nil?.should == false  
    end
  end
end