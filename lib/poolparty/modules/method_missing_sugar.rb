module PoolParty
  module MethodMissingSugar

    def method_missing(m, *args, &block)
      if block_given?
        (args[0].class == self.class) ? args[0].instance_eval(&block) : super
      else        
        get_from_options(m, *args, &block)
      end
    end
    
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