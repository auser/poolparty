module PoolParty
    
  class Pool < DslBase
    
    # Freeze the pool_name so we can't modify it at all
    # call and run instance_eval on the block and then call the after_create callback
    def initialize(n, &block)
      context_stack.clear
      
      @pool_name = n.to_s
      @pool_name.freeze
      
      callback :before_create
      super(&block)
      callback :after_create
    end
    
    # cloud
    # Define a cloud by a name and a block
    def cloud(name, o={}, &block)
      if block
        clouds[name.to_s] ||= PoolParty::Cloud.new(name, o, &block)
      else
        raise PoolPartyError.new("CloudError", "You must pass a block when defining a cloud")
      end
    end
    
    # Singleton methods
    
    # Load a clouds.rb
    def self.load_from_file(filename=nil)
      puts "Loading #{filename} from file in Pool"
      ddputs "Loading #{filename} from file in Pool"
      @clouds_dot_rb_file = filename
      before_specfile_load(clouds_dot_rb_file)
      o = File.open(filename, 'r') {|f| instance_eval f.read, clouds_dot_rb_file }
      after_specfile_load
      o
    end
    
    # Before the specfile is loaded this method is called
    # It...
    #   + loads the plugin paths local to the clouds_dot_rb_file into the load_path
    #   + calls the resource define_resource_methods to define the resource methods
    #   + sets up the log
    def self.before_specfile_load(filepath)      
      $:.unshift(::File.dirname(filepath))
      Dir["#{::File.dirname(filepath)}/plugins/*"].each { |plugin_path| $:.unshift(plugin_path) }
      PoolParty::Resource.define_resource_methods
      PoolParty::PoolPartyLog.init
    end
    
    # After the entire cloud is loaded
    def self.after_specfile_load
    end
    
  end
end