module PoolParty
  module Display
    
    def pool_describe(options={})
      if pools.size > 0
        pools.each do |k,v|
          print v.pretty_print
        end
      else
        prev = "\t"
        clouds.each do |name, cl|
          puts cl.pretty_name(prev*2, cl)
          puts cl.pretty_options(prev*2, cl) #cl.pretty_print("#{prev}\t")
        end
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