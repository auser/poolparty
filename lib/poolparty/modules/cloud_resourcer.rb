require "ftools"

module PoolParty
  module CloudResourcer
    
    def plugin_directory(*args)
      args = ["#{::File.expand_path(Dir.pwd)}/plugins"] if args.empty?
      args.each {|arg| Dir["#{arg}/*/*.rb"].each {|f| require f rescue "" }}
    end
    
    # Store block
    def store_block(&block)
      @stored_block ||= block ? block : nil
    end
    
    def stored_block
      @stored_block
    end
    
    # This will run the blocks after they are stored if there is a block
    # associated
    def run_stored_block
      self.run_in_context @stored_block if @stored_block
    end
    
    # Set instances with a range or a number
    def instances(arg)      
      case arg
      when Range
        minimum_instances arg.first
        maximum_instances arg.last
      when Fixnum
        minimum_instances arg
        maximum_instances arg
      else
        raise SpecException.new("Don't know how to handle instances cloud input #{arg}")
      end
    end
    
    def setup_dev
      return true if ::File.exists?("#{remote_keypair_path}") || master.nil?
      unless ::File.exists?("#{full_keypair_basename_path}.pub")
        cmd = "scp #{scp_array.join(" ")} #{Base.user}@#{master.ip}:.ssh/authorized_keys #{full_keypair_basename_path}.pub"
        vputs "Running #{cmd}"
        if %x[hostname].chomp == "master"
          Kernel.system("cat ~/.ssh/authorized_keys > #{full_keypair_basename_path}.pub")
        else
          Kernel.system(cmd)
        end        
      end
    end
    
    def full_keypair_path
      unless keypair_path
        raise RuntimeException.new("Keypair cannot be found")        
      else
        ::File.expand_path(keypair_path)
      end
    end
    def full_pub_keypair_path
      @full_pub_keypair_path ||= ::File.expand_path("#{full_keypair_basename_path}.pub")
    end
    def full_keypair_basename_path
      dir = ::File.dirname(full_keypair_path)
      basename = ::File.basename(full_keypair_path, ::File.extname(full_keypair_path))
      ::File.join(dir, basename)
    end
    
    def keypair_path
      keypair_paths.each do |path|
        possible_keypair_basenames.each do |base|
          full_path = ::File.join( File.expand_path(path), "#{base}#{keypair}")
          return full_path if ::File.exists?(full_path)
        end
      end
      return nil
    end
    
    # The keypair name can be one name or another including id_rsa or not
    # So let's get the name that exists as a keypair
    def full_keypair_name
      keypair_paths.each do |path|
        possible_keypair_basenames.each do |base|
          full_path = ::File.join( File.expand_path(path), "#{base}#{keypair}")
          return "#{base}#{keypair}" if ::File.exists?(full_path)
        end
      end
      return nil
    end
    
    def remote_keypair_path
      ::File.join( keypair_paths.last, "#{possible_keypair_basenames.first}#{keypair}" )
    end
    def new_keypair_path
      ::File.join( keypair_paths.first, "#{possible_keypair_basenames.first}#{keypair}" )
    end
    
    def possible_keypair_basenames
      [
        "id_rsa-",
        ""
      ]
    end
    
    def keypair_paths
      [
        Base.base_keypair_path,
        Base.remote_storage_path,
        Base.base_config_directory
      ]
    end
    
    def context_stack
      @@context_stack ||= []
    end
    
    def run_setup(parent, should_set_parent=true, &block)
      context_stack.push parent
      
      set_parent if should_set_parent
      run_in_context self, &block if block
      
      context_stack.pop
    end
    
    # Set the parent on the resource
    def set_parent(sink_options=true)
      unless context_stack.last.nil?
        @parent = context_stack.last
        # Add self as a service on the parent
        parent.add_service(self) if parent.respond_to?(:add_service)
        # Take the options of the parents
        configure(parent.options) if parent && parent.respond_to?(:options) && sink_options
      end
    end
            
    def number_of_resources
      arr = resources.map do |n, r|
        r.size
      end
      resources.map {|n,r| r.size}.inject(0){|sum,i| sum+=i}
    end
    
    def parent
      @parent ||= nil
    end
    
    def plugin_store
      @plugin_store ||= []
    end
    
    def realize_plugins!(force=false)
      plugin_store.each {|plugin| plugin.realize!(force) if plugin }
    end
    
    def plugin_store
      @plugins ||= []
    end
    
  end
end