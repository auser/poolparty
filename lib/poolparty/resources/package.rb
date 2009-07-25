module PoolParty
  module Resources
    
    class Package < Resource
      
      default_options(
        :action         => :install,
        :version        => nil,
        :response_file  => nil,
        :source         => nil,
        :options        => nil
      )
      
      def print_to_chef
        str = <<-EOE
package "<%= name %>" do
  action :<%= (action ? action : (exists ? :install : :remove)) %>
EOE
        str << "  options <%= print_variable(options) %>\n" if options
        str << "  version <%= print_variable(version) %>\n" if version
        str << "  source <%= print_variable(source) %>\n" if source
        str << "  response_file <%= print_variable(response_file) %>\n" if response_file
        str << "end"
      end
      
    end
    
  end
end