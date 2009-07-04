module PoolParty
  module Resources
    
    class FileResource < Resource
      
      def self.has_method_name
        "file"
      end
            
      def print_to_chef        
        <<-EOE
        template <%= name %> do
          <%- options.each do |k,v| %>
            <%= k -%> <%= v %>
          <% end %>        
        EOE
      end
      
    end
    
  end
end