require File.join(File.dirname(__FILE__), 'chef-deploy/git')
require File.join(File.dirname(__FILE__), 'chef-deploy/cached_deploy')

# deploy "/data/#{app}" do
#   repo "git://github.com/engineyard/rack-app.git"
#   branch "HEAD"
#   user "ez"
#   enable_submodules true
#   migrate true
#   migration_command "rake db:migrate"
#   environment "production"
#   shallow_clone true
#   action :deploy # or :rollback
#   restart_command "touch tmp/restart.txt"
# end

class Chef
  class Resource
    class Deploy < Chef::Resource
        
      def initialize(name, collection=nil, node=nil)
        super(name, collection, node)
        @resource_name = :deploy
        @deploy_to = name
        @branch = 'HEAD'
        @repository_cache = 'cached-copy'
        @copy_exclude = []
        @revision = nil
        @action = :deploy
        @allowed_actions.push(:deploy)
        @allowed_actions.push(:rollback)
      end
      
      def repo(arg=nil)
        set_or_return(
          :repo,
          arg,
          :kind_of => [ String ]
        )
      end
      
      
      def role(arg=nil)
        set_or_return(
          :role,
          arg,
          :kind_of => [ String ]
        )
      end
      
      def restart_command(arg=nil)
        set_or_return(
          :restart_command,
          arg,
          :kind_of => [ String ]
        )
      end
      
      def migrate(arg=nil)
        set_or_return(
          :migrate,
          arg,
          :kind_of => [ TrueClass, FalseClass ]
        )
      end
      
      def migration_command(arg=nil)
        set_or_return(
          :migration_command,
          arg,
          :kind_of => [ String ]
        )
      end
      
      def user(arg=nil)
        set_or_return(
          :user,
          arg,
          :kind_of => [ String ]
        )
      end
      
      def enable_submodules(arg=false)
        set_or_return(
          :enable_submodules,
          arg,
          :kind_of => [ TrueClass, FalseClass ]
        )
      end
      
      def shallow_clone(arg=false)
        set_or_return(
          :shallow_clone,
          arg,
          :kind_of => [ TrueClass, FalseClass ]
        )
      end

      def repository_cache(arg=nil)
        set_or_return(
          :repository_cache,
          arg,
          :kind_of => [ String ]
        )
      end
      
      def copy_exclude(arg=nil)
        set_or_return(
          :copy_exclude,
          arg,
          :kind_of => [ String ]
        )
      end
      
      def revision(arg=nil)
        set_or_return(
          :revision,
          arg,
          :kind_of => [ String ]
        )
      end
            
      def branch(arg=nil)
        set_or_return(
          :branch,
          arg,
          :kind_of => [ String ]
        )
      end
      
      def environment(arg=nil)
        set_or_return(
          :environment,
          arg,
          :kind_of => [ String ]
        )
      end
 
    end
  end
  
  class Provider
    class Deploy < Chef::Provider 
      
      def load_current_resource
        FileUtils.mkdir_p "#{@new_resource.name}/shared"
        FileUtils.mkdir_p "#{@new_resource.name}/releases"
        @dep = CachedDeploy.new  :user       => @new_resource.user,
                                :role       => @new_resource.role,
                                :branch     => (@new_resource.branch || 'HEAD'),
                                :restart_command => @new_resource.restart_command,
                                :repository => @new_resource.repo,
                                :environment => @new_resource.environment,
                                :migration_command => @new_resource.migration_command,
                                :migrate => @new_resource.migrate,
                                :deploy_to  => @new_resource.name,
                                :repository_cache  => @new_resource.repository_cache,
                                :copy_exclude  => @new_resource.copy_exclude,
                                :revision  => (@new_resource.revision || ''),
                                :git_enable_submodules => @new_resource.enable_submodules,
                                :git_shallow_clone  => @new_resource.shallow_clone,
                                :node => @node,
                                :new_resource => @new_resource
      end
      
      def action_deploy
        Chef::Log.level(:debug)
        Chef::Log.info "Running a new deploy\nto: #{@new_resource.name}\nrepo: #{@new_resource.repo}"
        @dep.deploy
        Chef::Log.level(Chef::Config[:log_level])
      end
      
      def action_rollback
        Chef::Log.level(:debug)
        Chef::Log.info "Rolling back deploy\nto: #{@new_resource.name}\nrepo: #{@new_resource.repo}"
        @dep.rollback
        Chef::Log.level(Chef::Config[:log_level])
      end
    end
  end
end

Chef::Platform.platforms[:default].merge! :deploy => Chef::Provider::Deploy, :gem_package => Chef::Provider::Package::Rubygems
