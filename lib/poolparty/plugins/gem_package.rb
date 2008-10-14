module PoolParty    
  class Gem
        
    virtual_resource(:gempackage) do
      
      # When we call gempackage, we want the exec to run on the directory we suggest
      # we also only want it to run if there is NOT a local gem already installed with
      # the package details (version and name)
      def loaded(opts={})
        has_exec(opts.merge({:name => "gem-package-#{name}", :cwd => "/tmp", :path => "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/var/lib/gems/1.8/bin"})) do
          command "gem install -y --no-ri --no-rdoc #{"--version #{version}" if @parent.version} #{"--source #{@parent.source}" if @parent.source} #{@parent.name}"
          ifnot "gem list --local #{@parent.name} | grep #{@parent.name} #{"| grep #{@parent.version}" if @parent.version}"
        end
      end
      
      # Because we are only running this one command, we want to make sure that when we
      # require it in another resource, that it is appropriately labeled
      def class_type_name
        "Exec"
      end
      # Additionally, because we change the name in the exec when required, we have to reflect
      # it here in the key. This is just so poolparty is aware of the resource when looking for 
      # it in a requires statement
      def key
        "gem-package-#{name}"
      end
    end
    
  end
end