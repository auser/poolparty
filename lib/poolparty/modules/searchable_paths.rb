module PoolParty
  # Abstracts out the searchable path for a resource such as a
  # template file, key, or clouds.rb
  module SearchablePaths
    def self.included(mod)
      mod.extend(ClassMethods)
    end

    module ClassMethods
      #
      # Options:
      # * <tt>:dirs</tt>: array of directories to look in *under* the search paths. (default: <tt>["/"]</tt>)
      # * <tt>:dir</tt>: set the directory to look in *under* the search paths. Use either dir or dirs, not both. (default: +/+)
      def has_searchable_paths(opts={})
        class_eval do
          extend PoolParty::SearchablePaths::SingletonMethods

          # setup class options
          @searchable_paths_dirs = [opts[:dir]] if opts[:dir]
          @searchable_paths_dirs = opts[:dirs]  if opts[:dirs]

        end
        include PoolParty::SearchablePaths::InstanceMethods
      end
    end

    module SingletonMethods
      def searchable_paths_dir;  @searchable_paths_dirs.first; end
      def searchable_paths_dirs
        @searchable_paths_dirs && @searchable_paths_dirs.size > 0 ? @searchable_paths_dirs : ["/"]
      end
     
      # Default locations to search for the key
      # def keypair_paths
      #   [ 
      #     "#{ENV["HOME"]}/.ssh",
      #     "#{Default.poolparty_home_path}/keys",
      #     PoolParty::Default.base_keypair_path,
      #     PoolParty::Default.base_config_directory,
      #     PoolParty::Default.base_ssh_path,
      #     PoolParty::Default.remote_storage_path,
      #     Dir.pwd
      #   ]
      # end

    end

    module InstanceMethods
   # Search for the key in default locations with the entire filepath
    # if the file exists. If it doesn't exist in the default locations, 
    # then it returns nil and assumes we it doesn't exist
    # def search_in_known_locations
    #   self.class.keypair_paths.each do |path|
    #     full_path = ::File.join( ::File.expand_path(path), ::File.basename(filepath))
    #     return full_path if ::File.exists?(full_path)
    #   end
    #   raise Exception.new("We cannot continue without a keypair. Please define a keypair in your clouds.rb")
    #   TODO: Add raise for keypair
    #   nil
    # end

    end

  end
end
