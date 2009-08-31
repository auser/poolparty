module PoolParty
  module Resources
  
    class Collectd < Resource
      
      PoolParty::Resources::FileResource.has_searchable_paths(:prepend_paths=> [File.dirname(__FILE__)+'/collectd/templates'])
      
      def after_loaded
        has_package 'collectd'
        %w(rrdtool librrd-dev librrd-ruby  libsensors-dev libsnmp-dev collectd collectd-dev).each{|pkg| 
          has_package pkg 
        }
        has_gem_package "astro-collectd"
        
        has_variable 'server', "localhost"
        has_file '/etc/collectd/collectd.conf' do
          template 'collectd.conf.erb'
        end
      end
      
    end
  
  end
end
