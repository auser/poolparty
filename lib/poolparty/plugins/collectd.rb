
module PoolParty
  module Resources
    FileResource.searchable_paths.unshift(File.dirname(__FILE__)+'/collectd/templates')
  
    class Collectd < Resource
      
      default_options :server => 'localhost', :base_dir => '/var/lib/collectd'
      
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
        has_variable 'base_dir', base_dir
        has_file '/etc/collectd/collectd.conf' do
          template 'collectd.conf.erb'
          # notifies get_service('collectd'), :restart
        end
      end
      
    end
    
  end
end
