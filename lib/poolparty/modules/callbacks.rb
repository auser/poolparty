module PoolParty
  module Callbacks
    module ClassMethods
      def additional_callbacks(arr=[])
        @additional_callbacks ||= arr
      end
    end
    
    module InstanceMethods
      def defined_callbacks
        %w(
            before_bootstrap
            after_bootstrap
            before_configure
            after_configure
          ) << self.class.additional_callbacks
      end
      
      # Callbacks on bootstrap and configuration
      def after_create
        defined_callbacks.each do |meth|          
          self.class.module_eval <<-EOE
            def call_#{meth}_callbacks(*args)
              plugin_store.each {|a| a.call_#{meth}_callbacks(*args) } if respond_to?(:plugin_store)
              self.send :#{meth}, *args if respond_to?(:#{meth})
            end
          EOE
        end
        super rescue nil
      end
    end
    
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods      
    end
  end
end