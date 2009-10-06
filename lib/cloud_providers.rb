=begin rdoc
  CloudProvider is the base class for cloud computing services such as Ec2, Eucalyptus - where your servers run.
=end
module CloudProviders

  # List of all defined cloud_providers
  def self.all
    @all ||= []
  end

end

%w(connections remote_instance cloud_provider).each do |lib|
  require File.dirname(__FILE__)+"/cloud_providers/#{lib}"
end

%w(ec2).each do |lib|
  require "cloud_providers/#{lib}/#{lib}"
end