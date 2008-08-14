module PoolParty
  module CustomFunction    
    class << self
      
      def has_custom_function(name)
        !output.select {|i| i =~ /#{name}/}.empty?
      end
      
      def append_if_no_such_line        
        output get_function_named(:append_if_no_such_line) unless has_custom_function("append_if_no_such_line")
      end
      
      def get_function_named(name)
        function_reference[:custom][name]
      end
      
      def function_reference
        @functions ||= YAML.load( File.join(File.dirname(__FILE__), "files", "custom_functions.yaml") )
      end
      
    end    
  end
end