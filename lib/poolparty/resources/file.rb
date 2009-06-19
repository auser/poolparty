module PoolParty
  module Resources
    
    class File < Resource
            
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