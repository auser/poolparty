module PoolParty

=begin  rdoc    
== Deploy Directory

The deploy directory will copy the source directory from the developer machine (i.e. your laptop) to /tmp/poolparty/cloudname, and then rsync it to the specified target directory on the cloud nodes.

== Usage

  has_deploy_directory('bob', 
                     :from => "~/path/to/my/site", 
                     :to => "/mnt",
                     :owner => 'www-data',
                     :git_pull_first => false  #do a git pull in the from directory before syncing

This will place the contents of ~/path/to/my/site from your machine to /mnt/bob on the cloud instances virtual_resource(:deploy_directory)

=end

  class DeployDirectory
    plugin(:deploy_directory) do
      
      dsl_methods :from, :to, :owner, :mode, :git_pull_first
      
      def loaded(opts={}, &block)        
        add_unpack_directory
      end
      
      def before_configure
        update_from_repo if git_pull_first
        package_deploy_directory
      end
      
      def package_deploy_directory
        ::Suitcase::Zipper.add("#{from}", "user_directory/")
      end
      
      def add_unpack_directory
        has_directory(to)
        has_exec("unpack-#{::File.basename(to)}-deploy-directory",
          :requires => get_directory(to),
          :command => "cp -R /var/poolparty/dr_configure/user_directory/#{dir_name}/* #{to}")
        if owner
          has_exec(:name => "chown-#{name}", :command => "chown #{owner} -R #{to}")
        end     

        if mode
          has_exec(:name => "chmod-#{name}", :command => "chmod #{mode} #{to}")
        end
      end
      
      def update_from_repo
        `cd #{from} && git pull`
      end
      
      private
      def dir_name
        ::File.basename(from)
      end
      
    end
  end
end