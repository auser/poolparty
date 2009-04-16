=begin rdoc
  DeployDirectory

  Deploy directory will rsync a local directory to 
  each instance of your cloud. 
  
  example:
    has_directory 'name', :from => '/local/path', :to => '/path/on/server/'
    
  The above example will place the contents of '/local/path' at '/path/on/server/name'
=end
module PoolParty
  class Deploydirectory
    
    virtual_resource(:deploy_directory) do
      
      def loaded(opts={}, &block)
        package_deploy_directory
        add_unpack_directory
      end
      
      def package_deploy_directory
        ::Suitcase::Zipper.add("#{from}", "user_directory/")
      end
      
      def add_unpack_directory
        has_directory("#{::File.dirname(to)}")
        has_exec("unpack-#{::File.basename(to)}-deploy-directory") do
          requires get_directory("#{::File.dirname(to)}")
          not_if "test -f #{to}"
          command "cp -R /var/poolparty/dr_configure/user_directory/#{name}/* #{to}"
        end
        
        if owner?
          has_exec(:name => "chown-#{name}") do
            command "chown #{owner} -R #{to}"
          end
        end
        
      end
      
    end
    
  end
end