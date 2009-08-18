=begin rdoc
  PoolPartyError
  
  Create an StandardError on the fly
=end
module PoolParty
  class PoolPartyError
    
    # Create an error with the class_name and error message
    # If the StandardError is not yet defined, define it, subclassing
    # StandardError and return the new class
    # Note: the class is set on Object
    def self.create(class_name="StandardError", msg="Error")
      Object.const_set(class_name, Class.new(StandardError)) unless Object.const_defined?(class_name)
      class_name.constantize.send(:new, msg)
    end
    
  end
end