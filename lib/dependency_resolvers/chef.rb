=begin rdoc
  Base dependency_resolver
=end
module PoolParty
  module DependencyResolvers
    
    class Chef < Base
      
      class << self
        attr_reader :meal
        
        def before_compile
          FileUtils.mkdir_p compile_directory unless ::File.directory?(compile_directory)
        end
        
        def after_compile
          compile_variables
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
        
        private
        
        def compile_variables
          FileUtils.mkdir_p compile_directory/"attributes" unless ::File.directory?(compile_directory/"attributes")
          File.open(compile_directory/"attributes"/"poolparty.rb", "w") do |f|
            f << "# variables\n"
            f << "poolparty Mash.new unless attribute?('poolparty')\n"
            variables.each do |var|
              f << "poolparty[:#{var.name}] = #{handle_print_variable(var.value)}\n"
            end
          end
        end
                
      end
            
    end
    
  end
end