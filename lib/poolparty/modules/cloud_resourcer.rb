require "ftools"

module PoolParty
  module CloudResourcer
    
    def plugin_directory(*args)
      args = ["/plugins"] if args.empty?
      args.each {|arg| Dir["#{arg}/*/*.rb"].each {|f| require f rescue "" }}
    end
    
    # Store block
    def store_block(&block)
      @stored_block ||= block
    end
    
    def run_stored_block      
      self.run_in_context @stored_block if @stored_block
    end
    
    # Set instances with a range
    def instances(arg)
      if arg.is_a?(Range)
        minimum_instances arg.first
        maximum_instances arg.last
      end
    end
    
    def full_keypair_path
      unless keypair_path
        raise RuntimeException.new("Keypair cannot be found")        
      else
        ::File.expand_path(keypair_path)
      end
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
        Base.base_config_directory,
        Base.remote_storage_path
      ]
    end
    
    # Set the parent on the resource
    def set_parent(pare, sink_options=true)
      unless pare == self
        @parent = pare
        # Add self as a service on the parent
        pare.add_service(self) if pare.respond_to?(:add_service)
        # Take the options of the parents
        configure(pare.options) if pare.respond_to?(:options) && sink_options
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
    
  end
end