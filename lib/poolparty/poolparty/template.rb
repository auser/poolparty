require "ostruct"
require "erb"

module PoolParty
  class Template
        
    def self.compile_string(string, opts={})
      vars = OpenStruct.new opts
      handle_render(string, vars, (opts.delete(:render) || :erb))
    end
    
    def self.compile_file(file, opts={})
      content = open(file).read
      compile_string(content, opts)
    end
    
    def self.handle_render(string, vars, renderer)
      case renderer
      when :haml
        nil
      else
        ERB.new(string).result(vars.send(:binding))
      end
    end
    
  end
end