module PoolParty
  module MethodMissingSugar
    def method_missing(m, *args, &block)
      if options.methods.include?("#{m}")
        args.empty? ? options.send(m,*args,&block) : options.send("#{m}=",*args,&block)
      else
        super
      end
    end
  end
end