module PoolParty
  
  module Resources
    
    class Apache < Resource
      
      default_options :port               => 80,
                      :www_user           => 'www-data',
                      :www_dir            => "/var/www",                      
                      :passenger_version  => "2.2.5"
      
      def before_load
        installed_as_worker
        configs
        has_service("apache2", :requires => get_package("apache2"))
        has_user "www-data"
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
          has_exec({:name => "reload-apache2", :command => "/etc/init.d/apache2 reload", :action => :nothing, :requires => get_package("apache2")})
          has_exec({:name => "force-reload-apache2", :command => "/etc/init.d/apache2 force-reload", :action => :nothing})
          @base_install = true
        end
      end
      
      
      def install_passenger
        enable_passenger
      end


   # LoadModule passenger_module /usr/lib/ruby/gems/1.8/gems/passenger-2.2.5/ext/apache2/mod_passenger.so
   # PassengerRoot /usr/lib/ruby/gems/1.8/gems/passenger-2.2.5
   # PassengerRuby /usr/bin/ruby1.8
   #
   # creating this thing below may not be being run b/c it checks for the passenger.conf which isn't really a good test
      
      def enable_passenger
        unless @enable_passenger
          installed_as_worker
          has_package     "build-essential"
          has_package     "apache2-prefork-dev"
          has_gem_package "fastthread"
          has_gem_package "passenger", :version => passenger_version
          passenger_configs
          
          has_exec "install_passenger_script" do
            command "passenger-install-apache2-module --auto"
            notifies get_exec("restart-apache2"), :run
            requires get_exec("restart-apache2")
            requires get_package("apache2")
            requires get_gem_package("passenger")
            not_if "test -e \#{node[:passenger_site][:passenger_module_path]}"
            # creates lambda { "passenger_site[:passenger_module_path]" }
            end
          
          @enable_passenger = true
        end
      end
      
      def passenger_configs
        unless @passenger_configs

          # requires doesn't work for has_variable?
          
          # has_variable("passenger_version",     passenger_version)
          # has_variable("passenger_root_path",   "\#{languages[:ruby][:gems_dir]}/gems/passenger-#{passenger_version}",
          #             :requires => get_variable("passenger_version"))
          # has_variable("passenger_module_path", "\#{passenger_site[:passenger_root_path]}/ext/apache2/mod_passenger.so", 
          #              :requires => get_variable("passenger_root_path"))

          has_variable("passenger_version",     passenger_version)
          has_variable("passenger_root_path",   "\#{languages[:ruby][:gems_dir]}/gems/passenger-#{passenger_version}",
                      :requires => get_variable("passenger_version"))
          has_variable("passenger_module_path", "\#{languages[:ruby][:gems_dir]}/gems/passenger-#{passenger_version}/ext/apache2/mod_passenger.so", 
                       :requires => get_variable("passenger_root_path"))
          
          has_file(:name => "/etc/apache2/mods-available/passenger.load") do
            content <<-eof
LoadModule passenger_module <%= @node[:passenger_site][:passenger_module_path] %>
            eof
            requires get_exec("install_passenger_script")
          end
          
          has_file(:name => "/etc/apache2/mods-available/passenger.conf") do
            content <<-eof
PassengerRoot <%= @node[:passenger_site][:passenger_root_path] %>
PassengerRuby <%= @node[:languages][:ruby][:ruby_bin] %>
            eof
            requires get_exec("install_passenger_script")
          end
          
          present_apache_module(:passenger, {:requires => get_file("/etc/apache2/mods-available/passenger.load")})
          @passenger_configs = true
        end
      end
      
      def configs
        unless @configs
          listen(port) unless @listen
          has_directory("#{www_dir}", :mode => "0755")
          has_directory("/etc/apache2")
          has_directory("/etc/apache2/conf.d")
          has_directory("/etc/apache2/site-includes")
          
          has_file("/etc/apache2/apache2.conf") do
            mode 0644
            requires get_directory("/etc/apache2/conf.d")
            requires get_package("apache2")
            template File.dirname(__FILE__)/"apache2"/"apache2.conf.erb"
          end
          # does_not_have_file(:name => "/etc/apache2/ports.conf")
          
          has_exec("/usr/sbin/a2dissite default") do
            only_if "/usr/bin/test -L /etc/apache2/sites-enabled/000-default"
            notifies get_exec("reload-apache2"), :run
            requires get_exec("reload-apache2")
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
        site "default-site", :template => File.dirname(__FILE__)/:apache2/"default-site.conf.erb"
      end
      
      def config(name, temp)
        has_file(:name => "/etc/apache2/conf.d/#{name}.conf") do
          template File.dirname(__FILE__)/temp
          notifies get_exec("reload-apache2"), :run
          requires get_exec("reload-apache2")
        end
      end
      
      def listen(p="80")
        has_variable(:name => "port", :value => [p])
        self.port = p
        @listen = true
      end
      
      def site(name, opts={})
        if exists?
          install_site(name, opts)
        else
          has_exec(:command => "/usr/sbin/a2dissite #{name}") do
            notifies get_exec("reload-apache2"), :run
            requires get_package("apache2")
            only_if "/bin/sh -c \"[ -L /etc/apache2/sites-enabled/#{name} ] && [ /etc/apache2/sites-enabled/#{name} -ef /etc/apache2/sites-available/#{name}]\""
          end
        end
      end
      
      def install_site(name, opts={})
        sitename = name

        opts.merge!(:name => "/etc/apache2/sites-available/#{sitename}", :requires => get_package("apache2"))
        has_directory(:name => "/etc/apache2/sites-available")
        has_file(opts, :requires => get_package("apache2")) unless opts[:no_file]
        has_exec(:name => "/usr/sbin/a2ensite #{sitename}") do
          notifies get_exec("reload-apache2"), :run
          requires get_exec("reload-apache2")
          requires get_file("/etc/apache2/sites-available/#{sitename}")
          not_if "/bin/sh -c '[ -L /etc/apache2/sites-enabled/#{sitename} ] && [ /etc/apache2/sites-enabled/#{sitename} -ef /etc/apache2/sites-available/#{sitename} ]'"
        end
      end
      
      def site_include(name, content, ensureer="present")
        has_file(:name => "/etc/apache2/site-includes/#{name}.inc", :ensures => ensureer, :content => content, :requires => get_file("/etc/apache2/site-includes"))
      end
      
      def present_apache_module(*names)
        opts = names.pop if names.last.kind_of?(::Hash)
        names.each do |name|
          has_exec(:name => "mod-#{name}", :command => "/usr/sbin/a2enmod #{name}") do            
            not_if "/bin/sh -c \'[ -L /etc/apache2/mods-enabled/#{name}.load ] && [ /etc/apache2/mods-enabled/#{name}.load -ef /etc/apache2/mods-available/#{name}.load ]\'"
            requires get_package("apache2")
            notifies get_exec("force-reload-apache2"), :run
            requires get_exec("force-reload-apache2")
            requires opts[:requires] if opts && opts[:requires]
          end
        end
      end
      
      def absent_apache_module(*names)
        names.each do |name|
          has_exec({:name => "no-mod-#{name}"}, :command => "/usr/sbin/a2dismod #{name}") do
            requires get_package("apache2")
            not_if "/bin/sh -c \'[ -L /etc/apache2/mods-enabled/#{name}.load ] && [ /etc/apache2/mods-enabled/#{name}.load -ef /etc/apache2/mods-available/#{name}.load ]\'"
            notifies get_exec("force-reload-apache2"), :run
            requires get_exec("force-reload-apache2")
          end
        end
      end
      
    end
    
  end
end

$:.unshift(File.dirname(__FILE__))
%w(php5 virtual_host passenger_site).each do |lib|
  require "apache2/#{lib}"
end
