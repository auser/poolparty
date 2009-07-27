module PoolParty
  module Resources
    
    class Line < Resource
      
      default_options(
        :file => nil,
        :line => ""
      )
      
      def self.has_method_name
        "line_in_file"
      end
      
      def filepath
        file || name
      end
      
      def after_loaded
        has_exec "line_in_#{filepath}" do
          command "grep -q \'#{line.safe_quote}\' #{filepath} || echo \'#{line.safe_quote}\' >> #{filepath}"
          not_if "grep -q \'#{line.safe_quote}\' #{filepath}"
        end
      end
      
      def print_to_chef
        <<-EOE
# line in file: <%= filepath %>
#   <%= line %>
<% ordered_resources.each do |res| %>
  <%= res.compile(:chef) %>
<% end %>
        EOE
      end
      
    end
    
  end
end