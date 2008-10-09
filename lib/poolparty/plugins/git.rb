module PoolParty    
  class Git
        
    virtual_resource(:git) do
      
      def loaded
        
        has_directory(:name => "#{name}", :path => "#{path}", :user => "#{user || Base.user}")
        
        exec({:name => "git-#{name}", :command => (user ? "git clone #{user}@#{source}" : "git clone #{source}")}) do
          cwd "#{path}"
          requires "File[#{name}]"
        end
        
        exec(:name => "git-update-#{name}", :cwd => "#{path}") do
          requires "Exec['git-#{name}']"
        end        
      end
    end
    
  end
end