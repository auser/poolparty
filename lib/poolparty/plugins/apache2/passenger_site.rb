module PoolParty
  module Resources
    # Usage:
    #
    # passenger_site do
    # end
    class PassengerSite < Resource
      
      default_options(
        :dir            => "/var/www",
        :appended_path  => nil,
        :owner          => 'www-data', 
        :mode           =>'0744',
        :environment    => 'production',
        :deploy_dirs    => false
      )
      
      def after_loaded(opts={})
        enable_passenger
        port "80" unless self.port
        
        has_directory(:name => dir,                 :owner => www_user, :mode => '0744')
        has_directory(:name => "#{site_directory}", :owner => www_user, :mode => '0744')
        has_site_directory 'logs'
        
        if dsl_options[:deploy_dirs] || opts[:with_deployment_directories]
          has_site_directory "shared"
          has_site_directory "shared/public"
          has_site_directory "shared/config"
          has_site_directory "shared/log"
          has_site_directory "releases"
          if !File.exists?("#{dir}/#{name}/current")
          
          # setup an initial symlink so apache will start even if there have not been any deploys yet
            has_site_directory "releases/initial/public"
            #FIXME  the following line is chef specific.  It will fail with puppet
            # has_symlink(:target_file => "#{dir}/#{name}/current", :to => "#{dir}/#{name}/releases/initial")
          end
          log_dir = "#{site_directory}/shared/log"
          appended_path "current"
        
        else
          log_dir = "#{site_directory}/log"
        end
        
        pass_entry = <<-EOE
  <VirtualHost *:#{port}>
      ServerName #{name}
      DocumentRoot #{site_directory}/public
      RailsEnv #{environment}
      ErrorLog #{log_dir}/error_log
      CustomLog #{log_dir}/access_log common
  </VirtualHost>
        EOE
        
        passenger_entry(pass_entry)
        
        install_site(name, :no_file => true) # we already created the file with #passenger_entry
      end
      
      def passenger_entry(file)
        if ::File.file?(file)
          has_file({:name => "/etc/apache2/sites-available/#{name}", :template => file})
        else
          has_file({:content => file, :name => "/etc/apache2/sites-available/#{name}" })
        end
      end
      
      def has_site_directory( dir_name='' , opts={})
        has_directory({ :name   => "#{site_directory}/#{dir_name}", 
                        :owner  => www_user, 
                        :mode   =>'0744'
                      }.merge(opts) )
      end
      
      def site_directory
        "#{dir}/#{name}%s" % [appended_path ? "/" + appended_path : ""]
      end
    end
    
  end
  
end