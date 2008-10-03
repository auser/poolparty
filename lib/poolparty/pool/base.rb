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
      :remote_storage_path => "/var/poolparty/files",
      :storage_directory => File.join(Dir.pwd, "tmp"),
      :fileserver_base => "puppet://puppet/",
      :base_config_directory => "/etc/poolparty",
      :access_key => ENV["AWS_ACCESS_KEY_ID"] ? ENV["AWS_ACCESS_KEY_ID"] : nil,
      :secret_access_key => ENV["AWS_SECRET_ACCESS_ID"] ? ENV["AWS_SECRET_ACCESS_ID"] : nil,
      :port => "8080"
    })
        
    # Class methods
    class << self
      def options(h={})
        @options ||= default_options.merge(h)
      end
    end
  end    
end