module PoolParty    
  module Resources
        
    class Gempackage < Resource
      
      # When we call gempackage, we want the exec to run on the directory we suggest
      # we also only want it to run if there is NOT a local gem already installed with
      # the package details (version and name)
      # 
      # TODO: Add it so that it tries to pull the gem off the master fileserver first...
      def loaded(opts={}, parent=self)
        if download_url
          
          has_file({
            :name => "#{Base.remote_storage_path}/#{name}.gem", 
            :source => "#{Base.fileserver_base}/#{name}.gem",
            :requires => get_host("master")
          })
                    
          has_exec(opts.merge({:name => "#{name}", :cwd =>"#{Base.remote_storage_path}", :path => "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/var/lib/gems/1.8/bin"})) do
            command "gem install -y --no-ri --no-rdoc #{Base.remote_storage_path}/#{name}.gem"
            ifnot "gem list --local #{name} | grep #{name} #{"| grep #{version}" if version}"
            requires get_file("#{Base.remote_storage_path}/#{name}.gem")
          end
          
          execute_if("$hostname", "master") do
            has_exec(:name => "download-#{name}", :cwd => Base.remote_storage_path, :command => "wget #{download_url} -O #{name}.gem", :ifnot => "test -f #{Base.remote_storage_path}/#{name}.gem")
          end
        else
          has_exec(opts.merge({:name => "#{name}", :cwd => "/tmp", :path => "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/var/lib/gems/1.8/bin"})) do
            command "gem install -y --no-ri --no-rdoc #{"--version #{version}" if version} #{"--source #{source}" if source} #{name} <<heredoc
            1
            heredoc"
            ifnot "gem list --local #{name} | grep #{name} #{"| grep #{version}" if version}"
          end
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