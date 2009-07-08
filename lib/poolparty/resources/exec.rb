module PoolParty
  module Resources
    
    class Exec < Resource
      
      dsl_methods :cwd, :command
      
      default_options(
        :path => ["/usr/bin:/bin:/usr/local/bin:$PATH"]
      )
            
      def print_to_chef
        <<-EOE
execute "<%= name %>" do
  command: "<%= command || name %>"
  path: ['<%= path.join(", ") %>']
end
        EOE
      end
      
    end
    
  end
end