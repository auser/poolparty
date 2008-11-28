module PoolParty
  module DependencyResolutions
    module Puppet
      
      def pretty_print_resources(pre=" ")
        returning Array.new do |out|
          resources.each do |name, res|
            out << "#{pre}#{name}"
            out << "#{pre*2}#{res.map {|a| a.name}}"
            res.each do |r|
              out << "#{pre*2}#{r.pretty_print_resources(pre*2)}"
            end
          end
        end.join("\n")
      end
      
      # Generic to_s
      # Most Resources won't need to extend this
      def to_string(pre="")
        opts = get_modified_options
        returning Array.new do |output|
          unless cancelled?
            output << @prestring || ""

            if resources && !resources.empty?
              output << resources_string_from_resources(resources, pre)
            end
            
            unless virtual_resource?
              output << "#{pre}#{class_type_name.downcase} { #{pre}\"#{self.key}\":"
              output << opts.flush_out("#{pre*2}").join(",\n")
              output << "#{pre}}"
            end
          
            output << @poststring || ""
          end
        end.join("\n")
      end
      
      def resources_string_from_resources(res, pre="\t")
        @variables = res.extract! {|name,resource| name == :variable}
        
        returning Array.new do |str|
          unless @variables.empty?
            str << "\n# Variables"
            @variables.each do |name, variable|
              str << variable.to_string("#{pre}")
            end          
          end

          res.reject do |type, resource|
            str << resource.to_string("#{pre*2}")
          end
        end.join("\n")
      end
      
      def to_s
        self.class.resource_string_name(class_type_name.capitalize, key)
      end
    end
  end
  
  module Resources
    class Resource
      def self.resource_string_name(name, key)
        "#{name}['#{key}']"
      end
    end
  end
end