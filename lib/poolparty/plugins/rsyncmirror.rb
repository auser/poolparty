module PoolParty    
  class Rsyncmirror
        
    virtual_resource(:rsyncmirror) do
      
      def loaded(opts={}, parent=self)
        @parent = parent        
        execute_on_master do
          @dir = dir || name
          has_exec(opts.merge({:command => "#{cloud.remote_rsync_command} --no-implied-dirs --delete-excluded #{Base.user}@master:#{@dir}/ #{@dir}/".safe_quote, :name => "rsync-#{name}"}))
        end
      end
            
      # Since git is not a native type, we have to say which core resource
      # it is using to be able to require it
      def class_type_name
        "exec"
      end
      
      # Because we are requiring an exec, instead of a built-in package of the git, we have to overload
      # the to_s method and prepend it with the same name as above
      def key
        "rsync-#{name}"
      end
      
    end
    
  end
end