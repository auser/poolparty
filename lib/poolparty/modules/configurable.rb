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
        options(h).merge!(h)
      end
      
      def reconfigure(h={})
        @options = nil
        options(h)
      end
      
      def set_vars_from_options(opts={})
        opts.each {|k,v| self.send k.to_sym, v } unless opts.empty?
      end
    end
    
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods
      receiver.send :include, MethodMissingSugar
    end
  end
end