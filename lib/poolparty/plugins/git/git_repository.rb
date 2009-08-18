=begin rdoc
speficy a git repo that should be checked out to all the nodes.
 
has_git_repository( :name       => "xnot",
                    :source     => "git://github.com/auser/xnot.org.git", 
                    :dir        => "/var/www",
                    :owner      => 'www-data')
=end

module PoolParty
  module Resources
    
    class GitRepository < Resource
      
      default_options(
        :name => nil,
        :repo => nil,
        :dir => nil,
        :owner => nil,
        :requires_user => nil,
        :deploy_key => nil,
        :source => nil
      )
      
      def after_loaded
        has_directory(::File.dirname(dir))
        has_directory(:name => "#{dir}", :requires => get_directory("#{::File.dirname(dir)}"))

        has_exec(:name => "git-#{name}", :creates => creates_dir ) do
          # Cloud, GitRepos, Exec
          if requires_user
            command("git clone #{requires_user}@#{repository} #{dir}")
          else
            command("cd #{dir} && git clone #{repository}")
          end

          cwd "#{dir if dir}"          
          requires get_directory("#{dir}")
        end
        
        has_exec(:name => "update-#{name}") do
          command "cd #{::File.dirname( creates_dir )} && git pull"
        end

        if owner
          has_exec(:name => "chown-#{name}", :cwd => ::File.dirname( creates_dir )) do            
            command "chown #{owner} * -R"
          end
        end

        if deploy_key
          raise Exception.new("Cannot find the git deploy key: #{deploy_key}") unless ::File.file?(::File.expand_path(deploy_key))
          ::Suitcase::Zipper.add(::File.expand_path(deploy_key), "keys")
          PoolParty::Provision::DrConfigure.class_commands << "cp -f /var/poolparty/dr_configure/keys/* ~/.ssh"
        end
      end
      
      private
      
      def to(d)
        dir d
      end
            
      def creates_dir
        @creates_dir = if source.include?(".git")
          "#{File.join( dir, File.basename(source, File.extname(source)) )}/.git"
        else
          "#{File.join( dir, File.basename(source) )}/.git"
        end
      end
      
      def repository(n=nil)
        if n
          self.repo = n
        else
          self.source ? self.source : name
        end
      end
      
    end
    
  end
end