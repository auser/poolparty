=begin rdoc
  Using method missing gives us the ability to set any attribute on the object into the options
  such that it can be retrieved later
=end
module PoolParty
  module MethodMissingSugar
    # Method_Missing
    # When a method cannot be found on the current object
    # it is sent to method_missing
    # First, we check if there is a block given and if the block
    # is given with a class of the same type, then it is a block to be evaluated on 
    # itself, so we instantiate it here, otherwise, don't handle it here and let
    # the super class worry about it.
    # If the block is not given, then we are going to look for it on the 
    # options of itself or its parent.
    # See get_from_options for more information
    def method_missing(m, *args, &block)
      if block_given?
        (args[0].class == self.class) ? args[0].instance_eval(&block) : super
      else        
        get_from_options(m, *args, &block)
      end
    end
    
    # Get the method from the options
    # First, we check to see if any options are being sent to the method
    # of the form:
    # name "Fred"
    # If there are args sent to it, check to see if it is an array
    # If it is, we want to send it an array only if the array is contains more than one element.
    # This becomes important when we are building the manifest
    # If it is not an array of more than one element, then we just send it the first of the args
    # If there are no args sent to it, then we check to see if the method is already in the options
    # which means we are retrieving the property
    # of the form
    # @cloud.name => @cloud.options[:name]
    
    def get_from_options(m, *args, &block)
      if args.empty?
        if options.has_key?(m)
          options[m]
        else
          (parent.nil? || parent.class == self.class || !parent.respond_to?(:options) || parent.options.has_key?(m)) ? nil : parent.send(m, *args, &block)
        end        
      else
        options[m] = (args.is_a?(Array) && args.size > 1) ? args : args[0]
      end
    end
    
  end
end