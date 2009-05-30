=begin rdoc
speficy a git repo that should be checked out to all the nodes.

has_git_repos(:name       => "xnot",
              :source     => "git://github.com/auser/xnot.org.git", 
              :dir        => "/var/www",
              :owner      => 'www-data',
              :deploy_key => 'pool_cloud.rsa')
=end
module PoolParty
  module Plugin
    class Git < Plugin
      def loaded(*args)
        has_package(:name => "git-core")
      end
    end
    
    class GitRepo < Plugin
      dsl_methods :name,
                  :repo,
                  :dir, 
                  :owner, 
                  :requires_user,
                  :deploy_key,
                  :source
                  
      def loaded(opts={}, &block)
        raise DirectoryMissingError.new unless dir
        
        has_package("git-core")
        has_git_repository
      end
      
      # retrieve/set source.  If source is not set, try and use name.
      def source(n=nil)
        n.nil? ? (dsl_options[:source] ? dsl_options[:source] : dsl_options[:source]= name) : dsl_options[:source]=n
      end

      def has_git_repository
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
          requires [get_directory("#{dir}"), get_package("git-core")]
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
      
      def to(d)
        dir d
      end
            
      def creates_dir
        "#{::File.join( dir, ::File.basename(name, ::File.extname(name)) )}/.git"
      end
      
      def repository(n=nil)
        if n
          self.repo = n
        else
          self.repo ? self.repo : name
        end
      end
      
    end
    
  end
end

class DirectoryMissingError < StandardError
  def initialize
    super("You must include a directory for the git repo set by to")
  end
end