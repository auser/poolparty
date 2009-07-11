=begin rdoc
  Base dependency_resolver
=end
module PoolParty
  module DependencyResolvers
    
    class Chef < Base
      
      def self.compile_resource(res)
        case res
        when Resources::Variable
          # do variable stuff
        else
          super
        end
      end
      
    end
    
  end
end