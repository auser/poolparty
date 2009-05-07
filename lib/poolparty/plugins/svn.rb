module PoolParty    
  class SvnResource
        
    virtual_resource :svn do
      def loaded(*args)
        has_package :name => "subversion"
      end
    end
    
    virtual_resource :svn_repos do
      
      dsl_methods :creates, :command, :cwd, :source, :working_dir, :at
      
      def loaded(opts={}, &block)
        has_package("subversion")
        has_svn_repository
      end

      def has_svn_repository
        has_directory(::File.dirname(working_dir))
        has_directory(:name => "#{working_dir}", :requires => get_directory("#{::File.dirname(working_dir)}"))
        
        has_exec(:name => "svn-#{name}", :requires => [get_directory("#{working_dir}"), get_package("subversion")] ) do
          cwd working_dir
          svn_cmd = if parent.requires_user?
            "svn co #{source} --username #{requires_user} --password #{requires_password}" 
          else
            "svn co #{source}"
          end
          command svn_cmd
          creates creates_dir
        end
        has_exec(:name => "update-#{name}", :cwd => ::File.dirname( creates_dir )) do
          command "svn up"
          # If the parent has after_update_svn set on it, then run it
          runs parent.after_update_svn if parent.after_update_svn?
        end
      end
      
      def svn_repos(src)
        source src
      end
      
      def at(dir)
        working_dir dir
      end
      
      def to(dir)
        at(dir)
      end
      
      def creates_dir
       "#{::File.join( working_dir, ::File.basename(source, ::File.extname(source)) )}/.svn"
      end
      
      # Since svn is not a native type, we have to say which core resource
      # it is using to be able to require it
      def class_type_name
        "exec"
      end
      
    end
    
  end
end