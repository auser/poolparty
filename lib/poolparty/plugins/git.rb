module PoolParty    
  class GitResource
        
    virtual_resource(:git) do
      
      def loaded(opts={}, parent=self)
        has_git_repos
      end
            
      def has_git_repos
        has_package(:name => "git-core")
        has_exec({:name => key, :requires => [get_directory("#{working_dir}"), get_package("git-core")] }) do
          command requires_user ? "git clone #{requires_user}@#{source} #{working_dir}" : "cd #{working_dir} && git clone #{source}"
          cwd "#{working_dir if working_dir}"
          creates creates_dir
        end
        has_exec(:name => "update-#{name}") do
          cwd ::File.dirname( creates_dir )
          command "git pull"
        end                
      end
      
      def at(dir)
        working_dir dir
        has_directory(:name => "#{dir}", :requires => get_directory("#{::File.dirname(dir)}"))
      end
      
      def to(dir)
        at(dir)
      end
      
      def creates_dir
        "#{::File.join( working_dir, ::File.basename(source, ::File.extname(source)) )}/.git"
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