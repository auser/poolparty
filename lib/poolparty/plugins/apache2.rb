module PoolParty
  class Base
    plugin :apache do
      
      # Called after the plugin is loaded entirely, all the options are set, etc.
      def loaded(o={}, &block)
        
        # We want to include the chef apache2 recipe to use it
        include_chef_recipe "#{::File.dirname(__FILE__)}/../../../vendor/chef/apache2"
        
        
      end
      
      def present_apache_module
        # Have to add virtual resource here (not yet complete)
        # something like
        # virtual_types << 
      end
      
      
    end
  end
end