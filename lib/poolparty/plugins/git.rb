module PoolParty    
  class GitResource
        
    virtual_resource(:git) do
      
      def loaded(opts={}, parent=self)
        has_git_repos
      end
            
      def has_git_repos
        has_package(:name => "git-core") do
          has_exec({:requires => get_package("git-core"), :requires => [get_directory("#{cwd}")]}) do
            name key
            command user ? "git clone #{user}@#{source} #{path}" : "git clone #{source} #{to ? to : ""}"
            cwd "#{cwd if cwd}"
            creates "#{::File.join( (cwd), ::File.basename(source, ::File.extname(source)) )}/.git"
          end
          has_exec(:name => "update-#{name}") do
            cwd ::File.dirname( get_exec(key).creates )
            command "git pull"
          end          
        end                
      end
      
      def at(dir)
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
        "git-#{name}"
      end
      
    end
    
  end
end