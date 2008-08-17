module PoolParty
  module MethodMissingSugar
    def method_missing(m, *args, &block)
      if options.keys.include?(m)
        args.empty? ? options[m] : (args.each {|a| options.store(m, a)} )
      else
        super
      end
    end
  end
end