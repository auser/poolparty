module PoolParty    
  class Git
        
    virtual_resource(:git) do
      
      def loaded(opts={}, parent=self)
        has_git_repos
      end
            
      def has_git_repos
        has_package(:name => "git-core")
        has_directory(:name => "#{cwd}")
        
        has_exec({:name => "git-#{name}", :requires => get_package("git-core"), :requires => [get_directory("#{cwd}"), get_package("git-core")]}) do          
          command parent.user ? "git clone #{parent.user}@#{parent.source} #{parent.path}" : "git clone #{parent.source} #{parent.to ? parent.to : ""}"
          cwd "#{parent.cwd if parent.cwd}"
          creates "#{::File.join( (parent.cwd ? parent.cwd : cwd), ::File.basename(parent.source, ::File.extname(parent.source)) )}/.git"
        end
        has_exec(:name => "update-#{name}", :requires => get_exec("git-#{name}")) do
          cwd get_exec("git-#{parent.name}").cwd
          command "git pull"
        end
      end
      
      # Since git is not a native type, we have to say which core resource
      # it is using to be able to require it
      def class_type_name
        "exec"
      end
      
      # Because we are requiring an exec, instead of a built-in package of the git, we have to overload
      # the to_s method and prepend it with the same name as above
      def key
        "git-#{name}"
      end
      
    end
    
  end
end