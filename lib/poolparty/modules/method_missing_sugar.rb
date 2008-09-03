module PoolParty
  module MethodMissingSugar
    def method_missing(m, *args, &block)      
      if block_given?
        (args[0].class == self.class) ? args[0].instance_eval(&block) : super
      else
        args.empty? ? options[m] : options[m] = args[0]
      end
    end
  end
  module MethodMissingHasResource
    def method_missing(m, options={}, &block)
      if m.to_s =~ /has_/
        self.send "#{m}".split("_")[-1].to_sym, options.merge(:ensure => "present"), &block
      elsif m.to_s =~ /does_not_have_/
        self.send "#{m}".split("_")[-1].to_sym, options.merge(:ensure => "absent"), &block
      else
        super
      end
    end
  end
end