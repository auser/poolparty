module PoolParty
  module Resources
    
    class Apache < Resource
      
      default_options :port               => 80,
                      :www_user           => 'www-data',
                      :passenger_version  => nil
      
      def after_loaded
        installed_as_worker
        configs
        has_service("apache2", :requires => get_package("apache2"))
      end
      
      def before_compile
        apache_configs
      end
      
      def installed_as_worker
        unless @installed_as_worker
          has_package("apache2")
          
          base_install
          @installed_as_worker = true
        end
      end
      
      def base_install
        unless @base_install
          has_exec({:name => "restart-apache2", :command => "/etc/init.d/apache2 restart", :action => :nothing})
          has_exec({:name => "reload-apache2", :command => "/etc/init.d/apache2 reload", :action => :nothing})
          has_exec({:name => "force-reload-apache2", :command => "/etc/init.d/apache2 force-reload", :action => :nothing})
          @base_install = true
        end
      end
      
      
      def install_passenger# {{{
        base_install 
        enable_passenger
      end# }}}
      
      def enable_passenger
        unless @enable_passenger
          installed_as_worker
          has_package     "build-essential"
          has_package     "apache2-prefork-dev"
          has_gem_package "fastthread"
          has_gem_package "passenger"
          passenger_configs
          
          has_exec(:name => "install_passenger_script", 
            :command => 'echo -en \"\\\\n\\\\n\\\\n\\\\n\" | passenger-install-apache2-module',
            :not_if => "test -f /etc/apache2/conf.d/passenger.conf && test -s /etc/apache2/conf.d/passenger.conf",
            :creates => lambda { "node[:poolparty][:passenger_module_path]" },
            :notifies => get_exec("restart-apache2")
            )
          
          @enable_passenger = true
        end
      end
      
      def passenger_configs
        unless @passenger_configs
          has_variable "gems_path", :value => lambda { "`gem env gemdir`.chomp!" }
          has_variable "ruby_path", :value => lambda { "`which ruby`.chomp!" }
          
          passenger_version ||= "2.2.2"
          
          has_variable("passenger_version",     :value => passenger_version)
          has_variable("passenger_root_path",   :value => "\#{poolparty[:gems_path]}/gems/passenger-#{passenger_version}")
          has_variable("passenger_module_path", :value => "\#{poolparty[:passenger_root_path]}/ext/apache2/mod_passenger.so")
          
          has_file(:name => "/etc/apache2/mods-available/passenger.load") do
            content <<-eof
              LoadModule passenger_module <%= @node[:poolparty][:passenger_module_path] %>
            eof
          end
          
          has_file(:name => "/etc/apache2/mods-available/passenger.conf") do
            content <<-eof
              PassengerRoot <%= @node[:poolparty][:passenger_root_path] %>
              PassengerRuby <%= @node[:poolparty][:ruby_path] %>
            eof
          end
          
          present_apache_module(:passenger)
          @passenger_configs = true
        end
      end
      
      def configs
        unless @configs
          listen(port) unless @listen
          has_directory("/etc/apache2")
          has_directory("/etc/apache2/conf.d")
          has_directory("/etc/apache2/site-includes")
          
          has_file(:name => "/etc/apache2/apache2.conf") do
            mode 0644
            requires get_directory("/etc/apache2/conf.d")
            template File.dirname(__FILE__)/"apache2"/"apache2.conf.erb"
          end
          # does_not_have_file(:name => "/etc/apache2/ports.conf")
          
          has_exec("/usr/sbin/a2dissite default") do
            only_if "/usr/bin/test -L /etc/apache2/sites-enabled/000-default"
            notifies get_exec("reload-apache2")
          end
          
          # Base config
          config("base",          "apache2"/"base.conf.erb")
          config("mime",          "apache2"/"mime-minimal.conf.erb")
          config("browser_fixes", "apache2"/"browser_fixes.conf.erb")
          
          present_apache_module("mime", "rewrite")
        # end
          @configs = true
        end
      end
      
      def apache_configs
        has_chef_attributes_file PoolParty.lib_dir/"vendor"/"chef"/"apache2"/"attributes"/"apache.rb"
      end
      
      def enable_default
        listen 80 # assumes no haproxy
        site "default-site", :template => :apache2/"default-site.conf.erb"
      end
      
      def config(name, temp)
        has_file(:name => "/etc/apache2/conf.d/#{name}.conf") do
          template File.dirname(__FILE__)/temp
          notifies get_exec("reload-apache2")
        end
      end
      
      def listen(p="80")
        has_variable(:name => "port", :value => p)
        self.port = p
        @listen = true
      end
      
      def site(name, opts={})
        case opts[:ensure] || "present"
        when "present", "installed"
          install_site(name, opts)
        when "absent"
          has_exec(:command => "/usr/sbin/a2dissite #{name}", :notifies => get_exec("reload-apache2")) do
          requires get_package("apache2")
            only_if "/bin/sh -c \"[ -L /etc/apache2/sites-enabled/#{name} ] && [ /etc/apache2/sites-enabled/#{name} -ef /etc/apache2/sites-available/#{name}]\""
          end
        end
      end
      
      def install_site(name, opts={})
        opts.merge!(:name => "/etc/apache2/sites-available/#{name}")
        has_directory(:name => "/etc/apache2/sites-available")
        has_file(opts) unless opts[:no_file]
        has_exec(:name => "/usr/sbin/a2ensite #{name}", :notifies => get_exec("reload-apache2")) do
          requires get_package("apache2")
          not_if "/bin/sh -c '[ -L /etc/apache2/sites-enabled/#{name} ] && [ /etc/apache2/sites-enabled/#{name} -ef /etc/apache2/sites-available/#{name} ]'"
        end
      end
      
      def site_include(name, content, ensureer="present")
        has_file(:name => "/etc/apache2/site-includes/#{name}.inc", :ensures => ensureer, :content => content, :requires => get_file("/etc/apache2/site-includes"))
      end
      
      def present_apache_module(*names)
        names.each do |name|
          has_exec(:name => "mod-#{name}", :command => "/usr/sbin/a2enmod #{name}") do
            requires get_package("apache2")
            not_if "/bin/sh -c \'[ -L /etc/apache2/mods-enabled/#{name}.load ] && [ /etc/apache2/mods-enabled/#{name}.load -ef /etc/apache2/mods-available/#{name}.load ]\'"
            notifies get_exec("force-reload-apache2")            
          end
        end
      end
      
      def absent_apache_module(*names)
        names.each do |name|
          has_exec({:name => "no-mod-#{name}"}, :command => "/usr/sbin/a2dismod #{name}") do
            requires get_package("apache2")
            not_if "/bin/sh -c \'[ -L /etc/apache2/mods-enabled/#{name}.load ] && [ /etc/apache2/mods-enabled/#{name}.load -ef /etc/apache2/mods-available/#{name}.load ]\'"
            notifies get_exec("force-reload-apache2")
          end
        end
      end
      
    end
    
  end
end