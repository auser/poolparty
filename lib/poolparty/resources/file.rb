module PoolParty    
  module Resources
        
    class File < Resource
      
      def after_create
        puts "In after_create for File: #{dsl_options.keys.join(" ")}"
        if dsl_options.include?(:template)          
          filename = ::File.expand_path(dsl_options.template)
          dsl_options.delete(:template)
          file = ::File.basename( filename )
          raise TemplateNotFound.new("no template given") unless file

          template_opts = (parent ? options.merge(parent.options) : options)
          options.merge!(:content => Template.compile_file(filename, template_opts).gsub("\"", "\""))
        end
        if dsl_options.include?(:content)
          cont = dsl_options.delete(:content)
          template_opts = (parent ? options.merge(parent.options) : options)
          p Template.compile_string(cont, template_opts)
          options.merge!(:content => Template.compile_string(cont, template_opts))
        end
      end
      
    end
    
  end
end