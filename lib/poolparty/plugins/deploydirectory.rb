=begin rdoc
  DeployDirectory
  
  Deploy directory will tar.gz a local directory and sync it up to 
  the master instance of the cloud. This enables you to send a directory
  up to the cloud and let the master host it for the remote slaves
=end
module PoolParty
  class Deploydirectory
    
    virtual_resource(:deploydirectory) do
      
      def loaded(opts={})
        package_directory
        unpack_directory
        sync_directories
      end
      
      def package_directory
        path = ::File.join( Default.tmp_path, "#{::File.basename(from_dir)}.tar.gz" )
        archive_name = "#{::File.basename(name).dir_safe}.tar.gz"
        cmd = "cd #{::File.expand_path(from_dir)} && tar -czf #{archive_name} . && mv #{archive_name} #{Default.tmp_path}"
        Kernel.system(cmd) unless testing
      end
      
      def unpack_directory
        case_of "hostname"
        when_is "master" do
          has_exec({:name => "deploy-directory-#{name}", :requires => get_directory("#{cwd}"), :cwd => cwd}) do
            #  && rm #{Base.tmp_path}/#{parent.name.dir_safe}.tar.gz
            archive_name = "#{::File.basename(name).dir_safe}.tar.gz"
            command "cd #{cwd}; tar -zxf #{Default.remote_storage_path}/#{archive_name}; rm #{Default.remote_storage_path}/#{archive_name}; chown #{owner} #{::File.basename(name).dir_safe}"
            onlyif "test -f #{Default.remote_storage_path}/#{archive_name}"
          end
        end
        end_of
      end
      
      def sync_directories
        execute_on_node do
          has_rsyncmirror(:dir => cwd, :name => "deploydirectory-#{name}")
        end
      end
      
      def from(dir)
        from_dir (dir.include?(" ") ? dir.gsub(/[ ]/, '') : dir)
      end
      
      def to(dir)
        cwd dir
        name dir
        has_directory(:name => "#{dir}", 
                      :owner => owner,
                      :mode => mode)
      end
      
      # Since git is not a native type, we have to say which core resource
      # it is using to be able to require it
      def class_type_name
        "exec"
      end
      
      # Because we are requiring an exec, instead of a built-in package of the git, we have to overload
      # the to_s method and prepend it with the same name as above
      def key
        "deploy-directory-#{name}"
      end
      
    end
    
  end
end