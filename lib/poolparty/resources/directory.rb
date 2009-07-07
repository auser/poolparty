module PoolParty
  module Resources
    
    class Directory < Resource
      
      default_options(
        :recursive => true,
        :mode => "0644"
      )
            
      def print_to_chef        
        <<-EOE
directory "<%= name %>" do
  <%= print_dsl_options(':key: \":value\"') %>
end
        EOE
      end
      
    end
    
  end
end