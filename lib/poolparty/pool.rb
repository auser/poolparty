module PoolParty
    
  class Pool < DslBase
    
    default_options(
      :minimum_instances        => nil,     # minimum_instances default
      :maximum_instances        => nil     # maximum_instances default
    )
    
    # Freeze the pool_name so we can't modify it at all
    # call and run instance_eval on the block and then call the after_create callback
    def initialize(n, &block)
      PoolParty::Pool.init
      
      context_stack.clear
      
      @pool_name = n.to_s
      @pool_name.freeze
      
      super do
        instance_eval &block
        form_clouds
        clouds.each do |name, cld|
          cld.after_all_loaded
        end
        
      end
    end
    
    class << self;attr_accessor :command;end
    def command
      self.class.command
    end
    
    # cloud
    # Define a cloud by a name and a block
    def cloud(name, o={}, &block)
      if block
        clouds[name.to_s] ||= PoolParty::Cloud.new(name, soakable_options.merge(o), &block)
      else
        raise PoolPartyError.new("CloudError", "You must pass a block when defining a cloud")
      end
    end
    
    # Soaked options
    # These are options that can be set on the pool that will be grabbed from the pool
    # that can be set to the cloud. 
    # Soakable options:
    #   minimum_instances
    #   maximum_instances
    def soakable_options
      soaked_options = {}
      soaked_options.merge!(:minimum_instances => minimum_instances) if minimum_instances
      soaked_options.merge!(:maximum_instances => maximum_instances) if maximum_instances
      soaked_options
    end
    
    # Run twice to catch the errors on the first run
    # TODO: CHANGE ME!
    def form_clouds
      failed_clouds = []
      clouds.each do |name, cld|
        begin
          clouds[name].form_clouds
        rescue Exception => e
          failed_clouds << [name, cld]
          next
        end
      end
      failed_clouds.each do |name, cld|
        clouds[name].form_clouds
      end
    end
    
    # Singleton methods
    
    # Load a clouds.rb
    # Call the prerequisites (before_file_load)
    # and then instance_eval the contents
    # finally, call after_file_load callback after the
    # clouds.rb is loaded
    # Arguments:
    #   + file on the filesystem
    #   + open-uri url (http)
    def self.load_from_file(filename=nil)
      raise PoolPartyError.create("CloudsDotRbLoadError", "Cannot load the specified clouds.rb: #{filename}. Check to make sure it exists") unless filename && File.file?(filename)
      ddputs "Loading #{filename} from file in Pool"
      @clouds_dot_rb_file = filename
      before_file_load(clouds_dot_rb_file)
      o = instance_eval open(clouds_dot_rb_file).read, clouds_dot_rb_file
      after_file_load(clouds_dot_rb_file)
      o
    end
    
    # Store the clouds_dot_rb_file location
    def self.clouds_dot_rb_file(n=nil)
      if n
        @clouds_dot_rb_file = n
      else
        @clouds_dot_rb_file
      end
    end
    
    def self.clouds_dot_rb_dir(n=nil)
      self.clouds_dot_rb_file ? File.dirname(self.clouds_dot_rb_file) : "./"
    end
    
    # Load the default clouds.rb file
    # If a full filepath is given, then load the given path
    # if it is given, but not found or is not given entirely, then search the
    # following locations, in preferential order for the clouds_dot_rb_file and
    # load the first one found
    #   + CWD/clouds.rb
    #   + ENV["CLOUDS_DOT_RB"]
    #   + ENV["HOME"]/clouds.rb
    #   + /etc/poolparty/clouds.rb
    #   + /var/poolparty/clouds.rb
    def self.find_and_load_default_clouds_dot_rb(filename="clouds.rb")
      f = if File.file?(filename) 
          filename
        else
          find_default_clouds_dot_rb(filename)
        end
      ENV["CLOUDS_DOT_RB"] = f
      load_from_file(f)
    end
    
    # Look for the default clouds_dot_rb_file
    def self.find_default_clouds_dot_rb(filename)      
      path = default_clouds_dot_rb_locations.detect do |dir|
        File.file?(File.expand_path(dir / filename))
      end
      raise PoolPartyError.create("CloudsConfigFile", "Cannot find your config file") unless path && filename
      File.expand_path(File.join(path, filename))
    end
    
    # Default clouds_dot_rb_file locations
    #   + CWD/clouds.rb
    #   + ENV["CLOUDS_DOT_RB"]
    #   + ENV["HOME"]/clouds.rb
    #   + /etc/poolparty/clouds.rb
    #   + /var/poolparty/clouds.rb
    def self.default_clouds_dot_rb_locations
      @default_clouds_dot_rb_locations ||= [
        Dir.pwd,
        ENV["CLOUDS_DOT_RB"],
        PoolParty::Default.poolparty_home_path,
        PoolParty::Default.base_config_directory,
        PoolParty::Default.remote_storage_path
      ].flatten.reject {|a| a.nil?}
    end
    
    # Before the specfile is loaded this method is called
    # It...
    #   + loads the plugin paths local to the clouds_dot_rb_file into the load_path
    #   + calls the resource define_resource_methods to define the resource methods
    #   + sets up the log
    def self.before_file_load(filepath)
      $:.unshift(File.dirname(filepath))
      $:.unshift("#{File.dirname(filepath)}/lib")
      $:.unshift("#{File.dirname(filepath)}/plugins")
      
      Dir["#{File.dirname(filepath)}/lib/*.rb"].each {|lib_path| require lib_path }
      Dir["#{File.dirname(filepath)}/plugins/*"].each do |plugin_path|
        if File.directory?(plugin_path)
          $:.unshift(plugin_path)
        else
          require plugin_path if File.file?(plugin_path) && plugin_path.match(/.rb$/)
        end
      end
    end
    
    # Call init to the resource methods and init the log
    # It pulls the requires for chef
    # TODO: Pull require_chef_only_resources out
    def self.init
      DependencyResolvers::Chef.require_chef_only_resources
      PoolParty::Resource.define_resource_methods
      PoolParty::PoolPartyLog.init
    end
    
    # CALLBACKS
    # After the entire cloud is loaded
    def self.after_file_load(filepath)
    end
    
  end
end