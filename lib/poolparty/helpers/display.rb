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
      "#{Time.now} #{str}"
    end
    
    def subheader(str="")
      "#{Time.now} #{str} ________"
    end
    
  end
end