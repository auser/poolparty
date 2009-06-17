module PoolParty
  module Callbacks
    module ClassMethods
      def additional_callbacks(arr=[])
        @additional_callbacks ||= arr
      end
    end
    
    module InstanceMethods
      def defined_callbacks
        [
          :before_bootstrap,
          :after_bootstrap,
          :before_provision,
          :after_provision,
          :before_configure,
          :after_configure,
          :after_create,
          # TODO: Add after_launch_instance and after_terminate_instance
          :after_launch_instance,
          # :after_terminate_instance,
          self.class.additional_callbacks
        ].flatten
      end
      
      # Callbacks on bootstrap and configuration
      # Defines the callback accessors:
      #   call_before/after_bootstrap/configure_callbacks
      # 
      # When called, this method will first check to see if there 
      # are plugins and call those plugin's callbacks when called
      # The method (before/after_bootstrap/configure) is called
      # on self if the callback method is defined on self
      def setup_callbacks
        defined_callbacks.each do |meth|
          unless respond_to?("call_#{meth}_callbacks".to_sym)
            self.class.module_eval <<-EOE
              def call_#{meth}_callbacks(*args)
                if respond_to?(:plugin_store) && plugin_store
                  plugin_store.each do |a| 
                    a.call_#{meth}_callbacks(*args) if a.respond_to?(:call_#{meth}_callbacks)
                  end
                end
                self.send :#{meth}, *args if respond_to?(:#{meth})
              end
            EOE
          end
        end
      end
    end
    
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods      
    end
  end
end