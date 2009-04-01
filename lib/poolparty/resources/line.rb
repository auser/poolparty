module PoolParty    
  module Resources
        
    class LineInFile < Resource
      
      def file(i=nil)
        i ? options[:file] = i : options[:file]
      end
      
      def self.command(line, file)
        "grep -q \"#{line}\" #{file} || echo \"#{line.safe_quote}\" >> #{file}"
      end

    end
    
  end
end