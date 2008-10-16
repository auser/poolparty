module PoolParty    
  class Git
        
    virtual_resource(:git) do
      
      def loaded(opts={})
        install_git
        has_git_repos
      end
      
      def install_git
        has_package(:name => "git-core")
      end
      
      def has_git_repos
        with_options(:requires => 'Package["git-core"]') do
          # has_directory(:name => "#{path}")
                    
          exec({:name => "git-#{name}"}) do
            command @parent.user ? "git clone #{@parent.user}@#{@parent.source} #{@parent.path}" : "git clone #{@parent.source} #{@parent.path}"
            cwd "#{::File.dirname(@parent.path) if @parent.path}"
            creates "#{cwd}/.git"
          end

          exec(:name => "git-update-#{name}", :cwd => "#{path}") do
            command "git pull"
            requires "Exec['git-#{@parent.name}']"
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