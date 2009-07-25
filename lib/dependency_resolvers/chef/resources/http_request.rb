module PoolParty
  module Resources
    
    class HttpRequest < Resource
      
      default_options(
        :action   => :get,
        :message  => nil,
        :url      => nil
      )
      
      def print_to_chef
        <<-EOE
http_request "<%= name %>" do
  action :<%= action ? action : (exists ? :get : :delete) %>
  url <%= print_variable(url) %>
  message <%= print_variable(message || name) %>
end
        EOE
      end
      
    end
    
  end
end