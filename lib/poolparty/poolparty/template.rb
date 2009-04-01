require "ostruct"
require "erb"

module PoolParty
  class Template
        
    def self.compile_string(string, opts={})
      vars = OpenStruct.new opts
      ERB.new(string).result(vars.send(:binding))
    end
    
    def self.compile_file(file, opts={})
      content = open(file).read
      compile_string(content, opts)
    end
    
  end
end