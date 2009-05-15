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
    
    # Declare the remoter base
    # Check to make sure the available_bases is available, otherwise raise
    # Give access to the cloud the remote_base and instantiate a new
    # instance of the remote base
    def using(t, &block)
      return self.send t if self.respond_to? t
      if self.class.available_bases.include?(t.to_sym)
        klass_string = "#{t}".classify
        remote_base_klass = "::PoolParty::Remote::#{klass_string}".constantize      
        set_default_options(remote_base_klass.default_options)
        self.remote_base = remote_base_klass.send(:new, dsl_options, &block)
        self.remoter_base t.to_sym        
        instance_eval "def #{t};remote_base;end"
      else
        raise "Unknown remote base: #{t}"
      end
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
    
  end
end
