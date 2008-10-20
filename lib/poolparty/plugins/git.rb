module PoolParty    
  class Git
        
    virtual_resource(:git) do
      
      def loaded(opts={}, parent=self)
        install_git
      end
      
      def install_git
        has_package(:name => "git-core") do
          has_git_repos
        end
      end
      
      def has_git_repos
        exec({:name => "git-#{name}"}) do
          command @parent.user ? "git clone #{@parent.user}@#{@parent.source} #{@parent.path}" : "git clone #{@parent.source} #{@parent.to ? @parent.to : ""}"
          cwd "#{@parent.cwd if @parent.cwd}"
          creates "#{::File.join( (@parent.cwd ? @parent.cwd : cwd), ::File.basename(@parent.source, ::File.extname(@parent.source)) )}/.git"
          
          exec(:name => "update-#{name}") do
            cwd ::File.dirname(@parent.creates)
            command "git pull"
          end
          
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