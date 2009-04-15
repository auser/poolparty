module PoolParty    
  class GitResource
    
    virtual_resource(:git) do
      def loaded(*args)
        has_package(:name => "git-core")
      end
    end
    
    virtual_resource(:git_repos) do
      
      def loaded(opts={}, &block)        
        raise(Exception.new("You must include a directory for the git repos set by :at")) if at?.nil?
        # opts.has_key?(:at) ? at(opts.delete(:at)) : raise(Exception.new("You must include a directory for the git repos set by :at"))
        # opts.has_key?(:source) ? git_repos(opts.delete(:source) || opts[:name]) : raise(Exception.new("You must include the git source set by :source"))
        has_package("git-core")
        has_git_repository
      end

      def has_git_repository
        
        has_directory(::File.dirname(working_dir))
        has_directory(:name => "#{working_dir}", :requires => get_directory("#{::File.dirname(working_dir)}"))
        
        has_exec(:name => "git-#{name}", :requires => [get_directory("#{working_dir}"), get_package("git-core")] ) do
          # Cloud, GitRepos, Exec
          command parent.requires_user? ? "git clone #{requires_user}@#{source} #{working_dir}" : "cd #{working_dir} && git clone #{source}"
          cwd "#{working_dir if working_dir}"
          creates creates_dir
        end
        has_exec(:name => "update-#{name}", :cwd => ::File.dirname( creates_dir )) do          
          command "git pull"
        end
        
        if owner?
          has_exec(:name => "chown-#{name}", :cwd => ::File.dirname( creates_dir )) do
            command "chown #{owner} * -R"
          end
        end
        
        if deploy_key?
          raise Exception.new("Cannot find the git deploy key: #{deploy_key}") unless ::File.file?(::File.expand_path(deploy_key))
          ::Suitcase::Zipper.add(::File.expand_path(deploy_key), "keys")
          PoolParty::Provision::DrConfigure.class_commands << "cp -f /var/poolparty/dr_configure/keys/* ~/.ssh"
        end
        
      end
      
      def git_repos(src)
        source src
      end
      
      def at(dir)
        working_dir dir
      end
      
      def to(dir)
        at(dir)
      end
      
      def creates_dir
        "#{::File.join( working_dir, ::File.basename(source, ::File.extname(source)) )}/.git"
      end
      
    end
    
  end
end