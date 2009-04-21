=begin rdoc
  CloudResourcer provides the cloud with convenience methods
  that you can call on your cloud. This is where the 
  
    instances 2..10
    
  method is stored, for instance. It's also where the key convenience methods are written
=end
require "ftools"

module PoolParty
  module CloudResourcer
    
    def plugin_directory(arr=[])
      arr = [arr] if arr.is_a?(String)
      arr << [
        "#{::File.expand_path(Dir.pwd)}/plugins",
        "#{::File.expand_path(Default.poolparty_home_path)}/plugins"
      ]
      arr.flatten.each {|arg|    
        Dir["#{arg}/*/*.rb"].each {|f| require f } if ::File.directory?(arg)
      }
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
    # if passed with a hash, call nodes(hash) to return filtered list of 
    # instances
    def instances(arg)
      case arg
      when Range
        minimum_instances arg.first
        maximum_instances arg.last
      when Fixnum
        minimum_instances arg
        maximum_instances arg
      when Hash
        nodes(arg)
      else
        raise SpecException.new("Don't know how to handle instances cloud input #{arg}")
      end
    end
    
    def setup_dev
      return true if keypair || master.nil?
    end
    
    def using(t, &block)
      @cloud = self
      if t && self.class.available_bases.include?(t.to_sym)
        unless using_remoter?
          self.class.send :attr_reader, :remote_base
          self.class.send :attr_reader, :parent_cloud
          klass_string = "#{t}".classify
          klass = "::PoolParty::Remote::#{klass_string}".constantize
          
          @remote_base = klass.send :new, self, &block
          @remote_base.instance_eval &block if block          
          options[:remote_base] = klass.to_s if respond_to?(:options)
          
          @parent_cloud = @cloud
          instance_eval "def #{t};@remote_base;end"
        end
      else
        puts "Unknown remote base" 
      end
    end
    
    def using_remoter?
      @remote_base
    end
    
    # Keypairs
    # Use the keypair path
    def keypair(*args)
      if args && !args.empty?
        args.each {|arg| _keypairs.unshift Key.new(arg) unless arg.nil? || arg.empty? }
      else
        @keypair ||= _keypairs.select {|key| key.exists? }.first
      end
    end
    
    alias :set_keypairs :keypair
    
    def _keypairs
      dsl_options[:keypairs] ||= [Key.new]
    end
    
    def full_keypair_path
      @full_keypair_path ||= keypair.full_filepath
    end
    
    def update_from_schema(schema)
      keypairs = schema.options.delete(:keypairs).map {|a| PoolParty::Key.new(a.basename) }
      options.merge! schema.options
      dsl_options[:keypairs] = keypairs

      dsl_options[:dependency_resolver] = schema.options.dependency_resolver.split("::")[-1].gsub(/Resolver/, '').preserved_class_constant("Resolver") rescue PoolParty::Chef
      
    end
        
    # TODO: deprecate
    def number_of_resources
      arr = resources.map do |n, r|
        r.size
      end
      resources.map {|n,r| r.size}.inject(0){|sum,i| sum+=i}
    end
    
    def plugin_store
      @plugin_store ||= []
    end
    
    # TODO: deprecate
    def realize_plugins!(force=false)
      plugin_store.each {|plugin| puts "plugin: #{plugin}";plugin.realize!(force) if plugin }
    end
    
  end
end