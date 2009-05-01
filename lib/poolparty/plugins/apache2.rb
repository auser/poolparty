module PoolParty
  class Base
    plugin :apache do
      
      # Called after the plugin is loaded entirely, all the options are set, etc.
      def loaded(o={}, &block)
        include_chef_recipe chef_apache2_recipe_root # We want to include the chef apache2 recipe to use it
      end
      
      def present_apache_module(*names)
        names.each do |name|
          has_chef_recipe "apache2::mod_" + name
        end
      end

      private
      def chef_apache2_recipe_root
        "#{::File.dirname(__FILE__)}/../../../vendor/chef/apache2"
      end
      
    end


    # Usage:°
    #°
    # enable_php5 do
    #   extras :cli, :pspell, :mysql
    # end
    virtual_resource(:enable_php5) do
      def loaded(opts={}, parent=self)
        has_package("php5")
        has_package("libapache2-mod-php5")
        present_apache_module("php5")
        has_file({:name => "/etc/php5/apache2/php.ini",
                :template => File.dirname(__FILE__) + "/../templates/php.ini.erb",
                :mode => 755,
                :requires => get_package("libapache2-mod-php5")})
                # :notify => get_exec("reload-apache2")})
      end

      def extras(*names)
        names.each do |name|
          # has_package(:name => "php5-#{name}", :requires => get_package("php5"))
          has_package(:name => "php5-#{name}")
        end
      end

    end


  end

end