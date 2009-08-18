=begin rdoc
== ChefAttributesFile

Allows for an attributes file to be added to the chef recipe.

== Usage

  has_chef_attributes_file "full_path_to_attributes_file"

== Options

* <tt>file</tt> The location of the attributes file

== Examples

  has_chef_attributes_file PoolParty.lib_dir/"vendor"/"chef"/"apache2"/"attributes"/"apache.rb"
=end
module PoolParty
  module Resources
    
    class ChefAttributesFile < Resource
      
      default_options(
        :file    => nil
      )
      
      def valid?
        f = (file || name)
        raise PoolPartyError.create("ChefAttributesFileError", "You must specify a file that exists for a chef_attributes_file: #{f}") unless File.file?(full_path)
      end
      
      def path
        @path ||= file || name
      end
      
      def full_path
        File.expand_path(path)
      end
      
      def content
        open(path).read
      end
      
      def print_to_chef
        ""
      end
      
    end
    
  end
end