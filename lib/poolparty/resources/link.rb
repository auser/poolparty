module PoolParty
  module Resources
    
    class Link < Resource
      
      default_options(
        :link_type  => :symbolic,
        :to         => nil
      )
      
      def print_to_chef
        <<-EOE
link "<%= name %>" do
  link_type <%= print_variable(link_type) %>
  action :<%= exists? ? :create : :delete %>
  to <%= print_variable(to) %>
end
        EOE
      end
      
    end
    
  end
end