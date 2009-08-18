require "#{File.dirname(__FILE__)}/../../test_helper"

class CloudProviderTest < Test::Unit::TestCase
  
  def setup
    @provider = CloudProviders::CloudProvider.new
  end
  
  def test_cloud_providers_are_included_in_all
    assert_respond_to CloudProviders, :all
    assert_respond_to CloudProviders.all, :each
    assert CloudProviders.all.include?(CloudProviders::Ec2)
  end
  
  def test_responds_to_core_methods
    %w(describe_instances 
       describe_instance
       terminate_instance!
       run_instance
       ).each do |meth|
         assert_respond_to @provider, meth
       end
  end
  
end
