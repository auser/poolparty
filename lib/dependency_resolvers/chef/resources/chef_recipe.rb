=begin rdoc
== ChefRecipeFile

Allows for a chef recipe to be included in the PoolParty output

== Usage

  has_chef_recipe "full_path_to_attributes_file"

== Options

* <tt>file</tt> The directory of the recipe

== Examples

  has_chef_recipe PoolParty.lib_dir/"vendor"/"chef"/"apache2"/"attributes"/"apache.rb"
=end
module PoolParty
  module Resources
    
    class ChefRecipe < Resource
      
      default_options(
        :file    => nil
      )
      
      def valid?
        f = (file || name)
        raise PoolPartyError.create("ChefAttributesFileError", "You must specify a file that exists for a chef_attributes_file: #{f}") unless File.directory?(full_path)
      end
      
      def path
        @path ||= file || name
      end
      
      def full_path
        File.expand_path(path)
      end
      
      def basename
        File.basename(name)
      end
      
      def print_to_chef
        "recipe \"#{basename}\""
      end
      
    end
    
  end
end