=begin rdoc
  Recipe specification
  
=end
module Baker
  class Recipe < Template
    
    attr_accessor :string, :template_name
    
    def initialize(opts={})
      @options = extract_options(opts)
      @string = options[:string]
      @template_name = options[:template_name] || "default"
      @file = __FILE__ if string # If the string is set, then we want to fake the error produced by Template
      super
    end
    
    def compile
      ::FileUtils.mkdir_p full_path unless ::File.directory?(full_path)
      string ? File.open("#{full_path}/#{template_name}.erb", "a+") {|f| f << "#{string}\n"} : super
    end
    
    def template_path
      "recipes"
    end
    
  end
end