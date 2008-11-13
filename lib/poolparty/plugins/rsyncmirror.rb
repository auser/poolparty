module PoolParty    
  class Git
        
    virtual_resource(:rsyncmirror) do
      
      def loaded(opts={}, parent=self)
        has_rsync_mirror(opts, parent)
      end
            
      def has_rsync_mirror(opts={}, parent=self)
        has_exec(:command => "rsync -aRqv --no-implied-dirs --delete --delete-excluded '#{Base.user}@master:#{opts[:dir]}' '#{opts[:dir]}'", :name => "rsync-#{name}")
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