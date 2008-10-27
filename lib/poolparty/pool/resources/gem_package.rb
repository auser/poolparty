module PoolParty    
  module Resources
        
    class Gempackage < Resource
      
      # When we call gempackage, we want the exec to run on the directory we suggest
      # we also only want it to run if there is NOT a local gem already installed with
      # the package details (version and name)
      # 
      # TODO: Add it so that it tries to pull the gem off the master fileserver first...
      def loaded(opts={}, parent=self)
        # has_exec(:name => "unpack-#{name}-into-fileserver", :command => "gem unpack #{name} && mv #{name}* #{name}.gem", :cwd => Base.remote_storage_path) do
        #   ifnot "test -d #{Base.remote_storage_path}/#{name}.gem"
        # end
        
        has_exec(opts.merge({:name => name, :cwd => "/tmp", :path => "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/var/lib/gems/1.8/bin"}), parent) do          
          command "gem install -y --no-ri --no-rdoc #{"--version #{version}" if version} #{"--source #{source}" if source} #{name} <<heredoc
          1
          heredoc"
          ifnot "gem list --local #{name} | grep #{name} #{"| grep #{version}" if version}"
          # notify get_exec("unpack-#{name}-into-fileserver")
        end
      end
      def virtual_resource?
        true
      end
      def printable?
        true
      end
      def class_type_name
        "Exec"
      end
      
    end
    
  end
end