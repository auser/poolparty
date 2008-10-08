=begin rdoc
  Base
  This handles user interaction, loading the parameters, etc.
=end
require "open-uri"
module PoolParty
  class Base
    include Configurable
    extend MethodMissingSugar
    
    default_options({
      :environment => "production",
      :user => "root", # This should change here
      :base_keypair_path => "~/.ec2",
      :tmp_path => "tmp",
      :remote_storage_path => "/var/poolparty",
      :storage_directory => File.join(Dir.pwd, "tmp"),
      :fileserver_base => "puppet:///files",
      :base_config_directory => "/etc/poolparty",
      :template_directory => "tmp/templates",
      :template_path => "/var/lib/puppet/templates",
      :access_key => ENV["AWS_ACCESS_KEY_ID"] ? ENV["AWS_ACCESS_KEY_ID"] : nil,
      :secret_access_key => ENV["AWS_SECRET_ACCESS_ID"] ? ENV["AWS_SECRET_ACCESS_ID"] : nil,
      :port => "80",
      :forwarding_port => "8080",
      :proxy_mode => "http",
      :pool_logger_location => File.join(Dir.pwd, "logs"),
      # EC2 Options
      :ami => "ami-4bb05422" 
    })
        
    # Class methods
    class << self
      def options(h={})
        @options ||= default_options.merge(h)
      end
    end
  end    
end