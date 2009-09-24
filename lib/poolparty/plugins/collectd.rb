module PoolParty
  module Resources
  
    class Collectd < Resource
      
      default_options :server => 'localhost'
      
      PoolParty::Resources::FileResource.has_searchable_paths(:prepend_paths=> [File.dirname(__FILE__)+'/collectd/templates'])
      
      def after_loaded
        %w(rrdtool 
           librrd-dev 
           libsensors-dev
           libsnmp-dev
           collectd
           collectd-dev).each{|pkg|has_package pkg}
        # has_package librrd-ruby
        # has_gem_package "astro-collectd"
        has_variable 'server', server
        has_file '/etc/collectd/collectd.conf' do
          template 'collectd.conf.erb'
          notifies get_service('collectd'), :restart
        end
      end
      
    end
    
  end
end
