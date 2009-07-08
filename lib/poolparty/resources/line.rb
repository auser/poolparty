module PoolParty
  module Resources
    
    class Line < Resource
      
      default_options(
        :filepath => "",
        :line => ""
      )
      
      def self.has_method_name
        "line_in_file"
      end
      
      def after_loaded
        has_exec "line_in_#{filepath}" do
          command "grep -q \'#{line.safe_quote}\' #{file} || echo \'#{line.safe_quote}\' >> #{file}"
          not_if "grep -q \'#{line.safe_quote}\' #{file}"
        end
      end
      
    end
    
  end
end