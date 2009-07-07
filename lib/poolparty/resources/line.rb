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
        has_exec "line_in_#{filepath}", :command => "grep -q \'#{line.safe_quote}\' #{file} || echo \'#{line.safe_quote}\' >> #{file}", :not_if => "grep -q \'#{line.safe_quote}\' #{file}"
      end
            
      def print_to_chef        
        <<-EOE
template "<%= name %>" do
  content "<%= content %>"
end
        EOE
      end
      
    end
    
  end
end