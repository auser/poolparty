=begin rdoc
  Proxy printer object
  
  This stands in as the middle between the resource and the 
  dependency resolver and provides helper methds to the
  output
=end
module DependencyResolvers
  
  class ProxyObject
    attr_accessor :proxy
    attr_reader :current_printing_method, :caller
    
    def initialize(proxy, caller=nil)
      @proxy = proxy
      @caller = caller
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
        self.send(meth_name).to_s
      else
        raise PoolParty::PoolPartyError.create("ProxyObjectError", "Compilation of #{proxy.inspect} error. Strings and symbols are supported")
      end
      begin
        ERB.new(str).result(self.send(:binding))
      rescue Exception => e
        p [:error, e, str]
      end      
    end
    
    # Print the dsl options in the Erb string format
    # given by the method print_dsl_options(str)
    # To use print_dsl_options, the format is:
    # print_dsl_options("print :key = ':value'")
    # The string substitution uses the ^ substitution found in string.rb
    # This will substitute the key and the value in the format given by the 
    # string passed. For instance
    #   dsl_options = {:to => "world", :message => "hello"}
    #   print_dsl_options(":key => :value") =
    #     message => hello
    #     to => world
    # This should be used if all the dsl_options
    # are to printed in the same format
    def print_dsl_options(str)
      dsl_options.map do |k,v|
        v.nil? ? nil : str ^ {:key => k, :value => v}
      end.compact.join("\n")
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
    
    def instance
      @caller
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