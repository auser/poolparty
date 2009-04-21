module PoolParty
  
  # Allows you to specify a gem to be installed
  # You can optionally pass a :download_url if you want a specific gem or version installed
  # example:
  #   has_gem_package :name => 'rubot',  :download_url => 'http://rubyforge.org/frs/download.php/35089/rubot-base-0.0.1.gem'
  
  class GempackageResource
    
    virtual_resource(:gem_package) do
      
      def loaded(opts={}, &block)
        if download_url?
          has_exec(
            :name => "download-#{name}", 
            :cwd => Default.remote_storage_path, 
            :command => "wget #{download_url} -O #{name}.gem", 
            :if_not => "test -f #{Default.remote_storage_path}/#{name}.gem"
          )
          has_exec(
            :name => "install-#{name}-gem",
            :command => "gem install --no-ri --no-rdoc  #{Default.remote_storage_path}/#{name}.gem",
            :if_not => "gem list --local #{name} | grep #{name} #{"| grep #{version}" if version?}",
            :requires => "download-#{name}"
          )
        else
          has_exec(
            :name => "#{name}",
            :command  => "gem install --no-ri --no-rdoc #{"--version #{version}" if version?} #{"--source #{source}" if source?} #{name}",
            :if_not => "gem list --local #{name} | grep #{name} #{"| grep #{version}" if version?}"
          )
        end
      end
      
    end
  end
end