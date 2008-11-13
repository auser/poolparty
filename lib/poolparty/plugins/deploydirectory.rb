module PoolParty    
  class Deploydirectory
        
    virtual_resource(:deploydirectory) do
      
      def loaded(opts={}, parent=self)
        package_directory
        unpack_directory
      end
            
      def package_directory
        path = ::File.join( Base.tmp_path, "#{::File.basename(from_dir)}.tar.gz" )
        # cd /Users/auser/Sites/work/citrusbyte/internal/gems/pool-party/poolparty/spec/poolparty/plugins/ && tar -czf plugins.tar.gz . && mv plugins.tar.gz /tmp/poolparty && cd /tmp/poolparty
        cmd = "cd #{::File.expand_path(from_dir)} && tar -czf #{name.dir_safe}.tar.gz . && mv #{name.dir_safe}.tar.gz #{Base.tmp_path}"
        `#{cmd}` unless testing
      end
      
      def unpack_directory         
        has_exec({:name => "deploy-directory-#{name}", :requires => get_directory("#{cwd}"), :cwd => cwd}) do
          command "cd #{parent.cwd}; tar -zxf #{Base.tmp_path}/#{parent.name.dir_safe}.tar.gz && rm #{Base.tmp_path}/#{parent.name.dir_safe}.tar.gz"
        end
      end
      
      def from(dir)
        from_dir (dir.include?(" ") ? dir.gsub(/[ ]/, '') : dir)
      end
      
      def to(dir)
        cwd dir
        has_directory(:name => "#{dir}", :requires => get_directory("#{::File.dirname(dir)}"))
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