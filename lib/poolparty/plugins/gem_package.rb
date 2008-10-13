module PoolParty    
  class Gem
        
    virtual_resource(:gempackage) do
      
      def loaded
        has_exec(:name => "gem-package-#{name}", :cwd => "/tmp", :path => "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/var/lib/gems/1.8/bin") do
          command "gem install --no-ri --no-rdoc #{"--version \"#{version}\"" if @parent.version} #{"--source #{@parent.source}" if @parent.source} #{@parent.name}"
          ifnot "gem list --local #{@parent.name} | grep #{@parent.name} #{"| grep #{@parent.version}" if @parent.version}"
        end
      end
      
    end
    
  end
end