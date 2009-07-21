require File.dirname(__FILE__)+"/cloud_providers/cloud_provider"
require File.dirname(__FILE__)+"/cloud_providers/cloud_provider_instance"

=begin rdoc
  CloudProvider is the base class for cloud computing services such as Ec2, Eucalyptus - where your servers run.
=end
module CloudProviders
  # List all defined cloud_providers
  def self.all
    @all ||= []
  end
  
  # Declare the remoter base
  # Check to make sure the available_bases is available, otherwise raise
  # Give access to the cloud the remote_base and instantiate a new
  # instance of the remote base
  def using(t, o={}, &block)
    return @cloud_provider if @cloud_provider
    klass_string = "::CloudProviders::#{t.to_s.camelcase}"
    cloud_provider_klass = klass_string.constantize
    if CloudProviders.all.include?(cloud_provider_klass)
      set_default_options(cloud_provider_klass.default_options)
      @cloud_provider = cloud_provider_klass.send(:new, o.merge(:cloud=>self), &block)
      self.cloud_provider_name t.to_sym
      instance_eval "def #{t};@cloud_provider;end"
      
      # instance_eval "def run_instance(o={}); cloud_provider.run_instance;end"
      # instance_eval "def terminate_instance!(o={}); cloud_provider.terminate_instance!(o);end"
      # instance_eval "def describe_instances(o={}); cloud_provider.describe_instances;end"
      # instance_eval "def describe_instance(o={}); cloud_provider.describe_instance(o);end"
    else
      raise "Unknown cloud_provider: #{t}"
    end
  end
  
end

%w(ec2).each do |lib|
  require "cloud_providers/#{lib}/#{lib}"
end