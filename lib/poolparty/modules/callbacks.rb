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
          :before_configure,
          :after_configure,
          :after_create,
          # TODO: Add after_launch_instance and after_terminate_instance
          # :after_launch_instance,
          # :after_terminate_instance,
          self.class.additional_callbacks
        ].flatten
      end
      
      # Callbacks on bootstrap and configuration
      def setup_callbacks
        defined_callbacks.each do |meth|
          unless respond_to?("call_#{meth}_callbacks".to_sym)
            self.class.module_eval <<-EOE
              def call_#{meth}_callbacks(*args)
                plugin_store.each {|a| a.call_#{meth}_callbacks(*args) } if respond_to?(:plugin_store) && plugin_store
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