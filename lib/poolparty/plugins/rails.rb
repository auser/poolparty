module PoolParty
  module Resources
    
    class Rails < Resource
      
      default_options(
        :rails_version => "2.3.3",
        :deployer_user => false
      )
      
      def after_loaded
        has_package "libsqlite3-dev"

        has_gem_package "rails", :version => rails_version
        has_gem_package "sqlite3-ruby", :requires => get_package("libsqlite3-dev")
        
        has_user deployer_user, :home => "/home/#{deployer_user}" if deployer_user
      end
      
    end
    
  end
end

require "#{File.dirname(__FILE__)}/rails/app"