=begin rdoc
  CloudResourcer provides the cloud with convenience methods
  that you can call on your cloud. This is where the 
  
    instances 2..10
    
  method is stored, for instance. It's also where the key convenience methods are written
=end
require "fileutils"

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
    def using(t, o={}, &block)
      return self.send(t) if self.respond_to?(t)
      if ::PoolParty::Remote::RemoterBase.available_bases.include?(t.to_sym)
        klass_string = "#{t}".classify
        remote_base_klass = "::PoolParty::Remote::#{klass_string}".constantize
        
        @remote_base = remote_base_klass.send(:new, o.merge(:cloud=>self), &block)
        self.remoter_base t.to_sym
        instance_eval "def #{t};@remote_base;end"
        
        # instance_eval "def launch_new_instance!(o={}); remote_base.launch_new_instance!;end"
        # instance_eval "def terminate_instance!(o={}); remote_base.terminate_instance!(o);end"
        # instance_eval "def describe_instances(o={}); remote_base.describe_instances;end"
        # instance_eval "def describe_instance(o={}); remote_base.describe_instance(o);end"
      else
        raise "Unknown remote base: #{t}"
      end
    end
    
    def dependency_resolver(name=nil)
      if !name.nil?
        ext = name=~/Resolver$/  ? nil : 'Resolver'
        klass = ::PoolParty.module_eval("#{name.camelcase}#{ext}")
        raise DependencyResolverException.new("Unknown resolver") unless klass
        dsl_options[:dependency_resolver] = "#{name.camelcase}#{ext}"
        @dependency_resolver = klass
      else
        @dependency_resolver
      end
    end
        
    # def update_from_schema(schema)
    #   self.dsl_options.merge! schema.options.to_hash
    #   self.dependency_resolver = schema.options.dependency_resolver.split("::")[-1].gsub(/Resolver/, '').preserved_class_constant("Resolver") rescue PoolParty::Chef
    #   keypair schema.options.keypair_name
    #   remote_base_class = PoolParty::Remote.module_eval( schema.options.remoter_base.camelcase )
    #   self.remote_base = remote_base_class.new  schema.options.remote_base.to_hash
    #   
    #   # self.keypair = PoolParty::Key.new schema.options.keypair.basename
    #   # keypair = schema.options.delete(:keypairs).map {|a| PoolParty::Key.new(a.basename) }
    #   # options.merge! schema.options.to_hash
    #   # dsl_options[:keypairs] = keypairs
    #   # 
    #   # dsl_options[:dependency_resolver] = schema.options.dependency_resolver.split("::")[-1].gsub(/Resolver/, '').preserved_class_constant("Resolver") rescue PoolParty::Chef
    #   
    # end
    
  end
end
