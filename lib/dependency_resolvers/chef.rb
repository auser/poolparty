=begin rdoc
  Base dependency_resolver
=end
module PoolParty
  module DependencyResolvers
    
    class Chef < Base
      
      class << self
        attr_reader :meal
        
        def before_compile
          ::FileUtils.mkdir_p compile_directory unless ::File.directory?(compile_directory)
        end
        
        def after_compile
        end
                
        def compile_resource(res)
          case res
          when Resources::Variable
            # do variable stuff
            variables << res
          else
            super
          end
        end
        
        def variables
          @variables ||= []
        end
        
      end
            
    end
    
  end
end