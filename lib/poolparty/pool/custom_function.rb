module PoolParty
  module CustomFunction    
          
    def has_custom_function(name)
      !output.select {|i| i =~ /#{name}/}.empty?
    end
    
    def get_function_named(name)
      function_reference[name.to_sym]
    end
    
    def function_reference
      @functions ||= YAML.load( open(File.join(File.dirname(__FILE__), "..", "files", "custom_functions.yaml")).read )
    end
    
    def method_missing(m,*args,&block)
      begin
        output get_function_named(m) unless has_custom_function("#{m}")
      rescue
        super
      end
      
    end
    
  end    
end