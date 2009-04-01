module PoolParty    
  # module Resources
        
    class Gempackage < Service
      
      # When we call gempackage, we want the exec to run on the directory we suggest
      # we also only want it to run if there is NOT a local gem already installed with
      # the package details (version and name)
      # 
      # TODO: Add it so that it tries to pull the gem off the master fileserver first...
      def loaded(opts={})
        if download_url
                    
          case_of "hostname"
          when_is "master" do
            has_exec(:name => "download-#{name}", :cwd => Default.remote_storage_path, :command => "wget #{download_url} -O #{name}.gem", :ifnot => "test -f #{Default.remote_storage_path}/#{name}.gem")
          end
          end_of
          
          has_file({
            :name => "#{Default.remote_storage_path}/#{name}.gem", 
            :source => "#{Base.fileserver_base}/#{name}.gem",
            :requires => get_host("master")
          })
                    
          has_exec(opts.merge({:name => "#{name}", :cwd =>"#{Default.remote_storage_path}"})) do
            command "gem install --no-ri --no-rdoc #{Default.remote_storage_path}/#{name}.gem"
            ifnot "gem list --local #{name} | grep #{name} #{"| grep #{version}" if version}"
            requires get_file("#{Default.remote_storage_path}/#{name}.gem")
          end
          
        else
          has_exec(opts.merge({:name => "#{name}", :cwd => "/tmp", :path => "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/var/lib/gems/1.8/bin"})) do
            command "gem install --no-ri --no-rdoc #{"--version #{version}" if version} #{"--source #{source}" if source} #{name}"
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
    
  # end
end