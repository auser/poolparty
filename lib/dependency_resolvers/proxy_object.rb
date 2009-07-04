=begin rdoc
  Proxy printer object
  
  This stands in as the middle between the resource and the 
  dependency resolver and provides helper methds to the
  output
=end
module PoolParty
  module DependencyResolvers
    
    class ProxyObject
      attr_reader :proxy, :current_printing_method
      
      def initialize(proxy)
        @proxy = proxy
      end
      
      # <tt>Compile</tt>
      # Arguments:
      #   * String
      #     If a string is passed, it is assumed to be an erb template and
      #     is rendered in the binding of this object (the proxy is contained)
      #   * Symbol
      #     If a symbol is passed, it is assumed to be a method, primarily
      #     used for <tt>print_to_<resolver_name></tt> methods
      # The output of compile is an erb template that is rendered in the context
      # of this proxy object
      def compile(meth_name)
        str = case meth_name
        when String
          meth_name
        when Symbol
          @current_printing_method = meth_name
          self.send(meth_name)
        else
          raise PoolParty::PoolPartyError.create("ProxyObjectError", "Compilation of #{proxy.inspect} error. Strings and symbols are supported")
        end        
        ERB.new(str).result(self.send(:binding))
      end
      
      # Take all the ordered_resources of the proxy object
      # and print them with the current_printing_method, aka :chef for print_to_chef
      # This creates a new proxy object with each resource and sends it
      # :compile with the current printing method, collects the output
      # and joins them with a newline
      def print_resources
        ordered_resources.map do |res|
          ProxyObject.new(res).compile(current_printing_method)
        end.join("\n")
      end
      
      # method_missing
      # Because this ProxyObject is responsible for proxying
      # methods to the proxy object, the method_missing method
      # is used to curry methods across
      def method_missing(m,*a,&block)
        proxy.send(m,*a,&block)
      end
      
    end
    
  end
end