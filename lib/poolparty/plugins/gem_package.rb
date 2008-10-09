module PoolParty    
  class Gem
        
    virtual_resource(:gempackage) do
      
      def loaded        
        @version_str = "--version \"#{version}\"" if version
        @version_grep_str = "| grep #{version}"
        @source_str = "--source \"#{source}\""
        
        has_exec(:name => "gem-package-#{name}", :cwd => "/tmp", :path => "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/var/lib/gems/1.8/bin") do
          command "gem install #{@version_str} #{@source_str} #{name}"
          ifnot "gem list --local #{name} | grep #{name} #{@version_grep_str}"
        end
      end
      
    end
    
  end
end