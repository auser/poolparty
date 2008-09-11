module PoolParty
  module Display
    
    def pool_describe(options={})
      pools.each do |k,v|
        print v.pretty_print
      end
      puts ""
      pools.size
    end    
    
  end
end