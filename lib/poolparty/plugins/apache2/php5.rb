require File.dirname(__FILE__)+'/apache'
module PoolParty
  module Plugin
    # Usage: 
    # 
    # enable_php5 do
    #   extras :cli, :pspell, :mysql
    # end
    class EnablePhp5 < Plugin
      def loaded(opts={}, parent=self)
        has_package(:name => "php5")
        has_package(:name => "libapache2-mod-php5")
        present_apache_module("php5")
        has_file({:name => "/etc/php5/apache2/php.ini",
                :template => "apache2/php.ini.erb",
                :mode => 755,
                :requires => get_package("libapache2-mod-php5"),
                :calls => get_exec("reload-apache2")})

        has_file(:name => "/etc/apache2/conf.d/enable-php.conf", 
                 :mode => 755,
                 :calls => get_exec("reload-apache2"),
                 :content => <<-eos 
                 AddHandler php5-script php
                 AddType text/html       php
                 eos
                 )
      end

      def extras(*names)
        names.each do |name|
         has_package(:name => "php5-#{name}")
        end
      end

    end

  end
  
end