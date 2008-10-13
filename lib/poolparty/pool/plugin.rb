module PoolParty
      
  module Plugin
    
    class Plugin
      include Configurable
      include CloudResourcer
      include Resources
      
      attr_accessor :parent
      class_inheritable_accessor :name
      
      default_options({})
      
      def initialize(parent=self, opts={}, &block)
        set_parent(parent)
        block ? instance_eval(&block) : enable
      end
      
      # Overwrite this method
      def enable
      end
            
      # def method_missing(m, *args, &block)
      #   if block_given?
      #     (args[0].class == self.class) ? args[0].instance_eval(&block) : super
      #   elsif parent && parent.respond_to?(m)
      #     parent.send m, *args, &block
      #   else
      #     get_from_options(m, *args)
      #   end
      # end
      # 
      # def get_from_options(m, *args)
      #   args.empty? ? options[m] : options[m] = args[0]
      # end
      
    end
    
  end
end