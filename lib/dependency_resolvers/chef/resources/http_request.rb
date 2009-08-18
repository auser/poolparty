=begin rdoc
== HttpRequest
  
  Allows you to send an http request to a url
 
== Usage
  
  has_http_request "http://google.com/", :message => "{q : 'search term'}"
  
== Options
* <tt>url</tt> The url to send the request to (this can also be the name)
* <tt>message</tt> The payload to deliver with the http_request
  
=end
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