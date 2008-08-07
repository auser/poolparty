module PoolParty  
  module Pool
    
    def pool(name="main", &block)
      pools[name] = Pool.new(name, &block)
    end

    def pools
      @@pools ||= {}
    end

    class Pool
      attr_accessor :name    
      include Cloud
      include MethodMissingSugar

      def initialize(name,&block)
        @name = name
        self.instance_eval &block
      end
      
      def options(h={})
        @options ||= {
          :plugin_directory => "plugins"
        }.merge(h).to_os
      end
      
      alias_method :configure, :options
            
    end
    
  end
end