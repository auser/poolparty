module PoolParty    
  module Resources
        
    class User < Resource      
      
      dsl_methods :name,        # Name of the user
                  :password,    # Password for the user
                  :home,        # Home directory
                  :gid,        # primary group for user
                  :authorized_keys
                  
                  
      default_options({
        :shell => "/bin/sh"
      })

      def loaded(o={})
        @group = dsl_options.delete(:group)
      end
      
      def after_create
        if authorized_keys
          cloud.has_file :name=>"/home/#{name}/.ssh/authorized_keys", :content => authorized_keys
        end
      end
      
      def present
        :create
      end

      def absent
        :remove
      end
      
    end
    
  end
end