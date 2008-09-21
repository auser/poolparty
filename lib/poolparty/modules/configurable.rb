module PoolParty
  module Configurable
    module ClassMethods      
      def default_options(h={})
        @default_options ||= h
      end
    end
    
    module InstanceMethods
      def options(h={})
        @options ||= self.class.default_options.merge(h)
      end

      def configure(h={})
        @options = nil
        options(h)
      end
    end
    
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods
      receiver.send :include, MethodMissingSugar
    end
  end
end