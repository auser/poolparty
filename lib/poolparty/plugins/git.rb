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
          has_directory(:name => "#{path}")
          
          @dir = directory(:name => "#{@parent.path}")
          @dir.cancel if @dir
                    
          exec({:name => "git-#{name}"}) do
            command @parent.user ? "git clone #{@parent.user}@#{@parent.source} #{@parent.path}" : "git clone #{@parent.source} #{@parent.path}"
            cwd "#{::File.dirname(@parent.path) if ::File.dirname(@parent.path)}"
            creates "#{@parent.path}/.git"
          end

          exec(:name => "git-update-#{name}", :cwd => "#{path}") do
            command "git pull"
            requires "Exec['git-#{@parent.name}']"
          end

          if symlink
            has_file(:name => "#{symlink}") do
              ensures @parent.path
            end
          end
        end
      end
      
    end
    
  end
end