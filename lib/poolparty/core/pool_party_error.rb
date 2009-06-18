module PoolParty
  class PoolPartyError < StandardError
    
    def initialize(class_name="StandardError", msg="Error")
      klass = if Object.const_defined?(class_name)
        Object.const_get(class_name)
      else
        Object.const_set(class_name, Class.new(StandardError))
      end
      klass.new(msg)
    end
    
  end
end