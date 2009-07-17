=begin rdoc
  Base dependency_resolver
=end
module PoolParty
  module DependencyResolvers
    
    class Chef < Base
      
      class << self
        attr_reader :meal
        
        def before_compile
          @meal = Baker::Meal.new(compile_directory)
        end
        
        def after_compile
          meal.compile
        end
                
        def compile_resource(res)
          case res
          when Resources::Variable
            # do variable stuff
            meal.attribute res
          else
            super
          end
        end
        
        def variables
          @variables ||= []
        end
        
        def compile_variables
          @attr = Baker::Attributes.new :cookbook_directory => "#{File.dirname(__FILE__)}/../test_dir", :basename => "poolparty"
        end        
        
      end
            
    end
    
  end
end