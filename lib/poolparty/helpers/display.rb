module PoolParty
  module Display
    
    def pool_describe(options={})
      pools.each do |k,v|
        print v.pretty_print
      end
      puts ""
      pools.size
    end
    
    def available_bases
      puts Remote.available_bases
    end
    
    def header(str="")
      "*** #{str}"
    end
    
    def subheader(str="")
      "****** #{str}"
    end
    
  end
end