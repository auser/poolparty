require File.dirname(__FILE__) + "/plugin_model"
require File.dirname(__FILE__) + "/resource"

module PoolParty    
  module Cloud
    def cloud(name=:main, &block)
      clouds.has_key?(name) ? clouds[name] : (clouds[name] = Cloud.new(name, self, &block))
    end

    def clouds
      $clouds ||= {}
    end    
    
    class Cloud
      attr_reader :name, :templates
      attr_accessor :parent
      include PoolParty::PluginModel
      include PoolParty::Resources
      include Configurable
      include PrettyPrinter
      include CloudResourcer      
      include Remote      
      
      default_options({
        :minimum_instances => 2,
        :maximum_instances => 4,
        :access_key => ENV["AWS_ACCESS_KEY"],
        :secret_access_key => ENV["AWS_SECRET_ACCESS"],
        :ec2_dir => ENV["EC2_HOME"],
        :keypair => (ENV["KEYPAIR_NAME"].nil? || ENV["KEYPAIR_NAME"].empty?) ? nil : ENV["KEYPAIR_NAME"],
        :ami => 'ami-44bd592d',
        :polling_time => "30.seconds"
      })
      
      def initialize(name, parent, &block)
        @name = name
        # this can be overridden in the spec, but ec2 is the default
        using :ec2 
        set_parent(parent) if parent
        self.instance_eval &block if block_given?
      end
      
      def set_parent(parent)
        @parent = parent
        configure(parent.options) if parent.respond_to?(:options)
      end
      
      # Keypairs
      # If the parent (pool) doesn't have a keypair defined on it, then generate one based on the 
      # pool_name and the cloud_name
      def keypair(*args)
        has_keypair? ? options[:keypair] : generate_keypair(*args)
      end
      def has_keypair?
        options.has_key?(:keypair) && options[:keypair]
      end
      def generate_keypair(*args)
        options[:keypair] = args.length > 0 ? args[0] : "#{@parent.name}_#{@name}"
      end
      
      # Configuration files
      def build_manifest
        returning String.new do |str|          
          resources.each do |name, resource|
            str << "# #{name}"
            str << resource.to_string("\t")
          end
        end
      end
    end
  end  
end