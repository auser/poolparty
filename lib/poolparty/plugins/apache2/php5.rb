module PoolParty
  module Resources
    # Usage: 
    # 
    # enable_php5 do
    #   extras :cli, :pspell, :mysql
    # end
    class Php < Resource
      def loaded(opts={}, parent=self)
        has_package(:name => "php5")
        has_package(:name => "libapache2-mod-php5")
        present_apache_module("php5")
        has_file( "/etc/php5/apache2/php.ini",
                :template => "apache2/php.ini.erb",
                :mode => 755,
                :requires => get_package("libapache2-mod-php5")) do
                  notifies get_exec("reload-apache2"), :run
                end

        has_file("/etc/apache2/conf.d/enable-php.conf", 
                 :mode => 755,                 
                 :content => <<-eos 
                 AddHandler php5-script php
                 AddType text/html       php
                 eos
                 ) do
                  notifies get_exec("reload-apache2"), :run
                 end
      end

      def extras(*names)
        names.each do |name|
         has_package(:name => "php5-#{name}")
        end
      end

    end

  end
  
end