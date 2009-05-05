module PoolParty
  # Abstracts out the searchable path for a resource such as a
  # template file, key, or clouds.rb
  module SearchablePaths
    def self.included(mod)
      mod.extend(ClassMethods)
    end

    module ClassMethods
      # Specify that a particular class has methods for searchable paths.
      # 
      # Options:
      # * <tt>:dirs</tt>: array of directories to look in *under* the search paths. (default: <tt>["/"]</tt>)
      # * <tt>:dir</tt>: set the directory to look in *under* the search paths. Use either dir or dirs, not both. (default: +/+)
      # * <tt>:paths</tt>: overwrite all default paths and set the paths to this array exactly
      # * <tt>:append_paths</tt>:  append these paths to any existing paths
      # * <tt>:prepend_paths</tt>: prepend these paths to any existing paths
      def has_searchable_paths(opts={})
        class_eval do
          extend PoolParty::SearchablePaths::SingletonMethods

          @searchable_paths_dirs = [opts[:dir]] if opts[:dir]
          @searchable_paths_dirs = opts[:dirs]  if opts[:dirs]
          @paths_override        = opts[:paths] if opts[:paths]
          @paths_prepend         = opts[:prepend_paths] || []
          @paths_append          = opts[:append_paths]  || []
        end
        include PoolParty::SearchablePaths::InstanceMethods
      end
    end

    module SingletonMethods
      def searchable_paths_dir;  @searchable_paths_dirs.first; end
      def searchable_paths_dirs
        @searchable_paths_dirs && @searchable_paths_dirs.size > 0 ? @searchable_paths_dirs : ["/"]
      end
     
      # The default paths are primarily defined in PoolParty::Default. These are the default search paths in order:
      # 
      # * current working directory (Dir.pwd)
      # * ~/.poolparty
      # * ~/.ec2
      # * /etc/poolparty
      # * /var/poolparty
      def default_paths
        [
          Dir.pwd,
          PoolParty::Default.poolparty_home_path,
          PoolParty::Default.base_keypair_path,
          PoolParty::Default.base_config_directory,
          PoolParty::Default.remote_storage_path
        ]
      end

      # returns the full set of valid searchable paths, given the options
      def searchable_paths
        return @paths_override if @paths_override && @paths_override.size > 0
        @paths_prepend + default_paths + @paths_append
      end
    end

    module InstanceMethods

      # Search for the file in path locations with the entire filepath
      # if the file exists. If it doesn't exist in the default locations, 
      # then it returns nil and assumes we it doesn't exist
      def search_in_known_locations(filepath)
        self.class.searchable_paths.each do |path|
          self.class.searchable_paths_dirs.each do |dir|
            full_path = File.expand_path(path) / dir / File.basename(filepath)
            return full_path if File.exists?(full_path)
          end
        end
        nil
      end
      alias_method :find_file, :search_in_known_locations

    end

  end
end
