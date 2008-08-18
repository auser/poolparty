module PoolParty    
  module Cloud
    def cloud(name=:main, &block)
      clouds.has_key?(name) ? clouds[name] : (clouds[name] = Cloud.new(name, self, &block))
    end

    def clouds
      @@clouds ||= {}
    end    
    
    class Cloud
      attr_reader :name, :templates
      attr_accessor :parent
      include MethodMissingSugar
      include Configurable
      
      default_options({
        :minimum_instances => 2,
        :maximum_instances => 4,
        :access_key => ENV["AWS_ACCESS_KEY"],
        :secret_access_key => ENV["AWS_SECRET_ACCESS"],
        :ec2_dir => ENV["EC2_HOME"],
        :keypair => (ENV["KEYPAIR_NAME"].nil? || ENV["KEYPAIR_NAME"].empty?) ? "pool" : ENV["KEYPAIR_NAME"],
        :ami => 'ami-44bd592d',
        :polling_time => "30.seconds"
      })
      
      def initialize(name, parent, &block)
        @name = name
        set_parent(parent)
        self.instance_eval &block if block_given?
      end
      
      def set_parent(parent)
        @parent = parent
        configure(parent.options)
      end
                  
    end
  end  
end