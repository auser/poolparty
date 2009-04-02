=begin rdoc
  DeployDirectory
  
  Deploy directory will tar.gz a local directory and sync it up to 
  the master instance of the cloud. This enables you to send a directory
  up to the cloud and let the master host it for the remote slaves
=end
module PoolParty
  class Deploydirectory
    
    virtual_resource(:deploy_directory) do
      
      def loaded(opts={}, &block)        
        # raise(Exception.new("You must include a directory for the git repos set by :at")) if at?.nil?
        # opts.has_key?(:at) ? at(opts.delete(:at)) : raise(Exception.new("You must include a directory for the git repos set by :at"))

        package_deploy_directory
        add_unpack_directory
      end
      
      def package_deploy_directory
        ::Suitcase::Zipper.add(sync_dir, "user_directory/")
      end
      
      def add_unpack_directory
        has_directory("#{::File.dirname(basedir)}")
        
        has_exec("unpack-#{::File.basename(basedir)}-deploy-directory") do
          requires get_directory("#{::File.dirname(basedir)}")
          cwd basedir
          onlyif "test -f #{basedir}/#{sync_dir}"
          command "cd #{cwd}; cp -R /var/poolparty/dr_configure/user_directory/#{sync_dir}; rm -rf /var/poolparty/dr_configure/#{name}"
        end
      end
            
      def from(dir)
        sync_dir dir
      end
      
      def to(dir)
        basedir dir
      end
      
    end
    
  end
end