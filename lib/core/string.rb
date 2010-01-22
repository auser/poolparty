class String
  # Turn a downcased string and capitalize it
  # so that it can be a class
  # doc_river #=> DocRiver
  def camelcase
    gsub(/(^|_|-)(.)/) { $2.upcase }
  end
  
  # "FooBar".snake_case #=> "foo_bar"
  def snake_case
   gsub(/\B[A-Z]+/, '_\&').downcase
  end
 
  # "FooBar".dasherize #=> "foo-bar"
  def dasherize
    gsub(/\B[A-Z]+/, '-\&').downcase
  end
    
  # Turn a string from lowercased with a .
  # to a classified classname
  # rice_and_beans #=> "RiceAndBeans"
  # handles subclassed and namespaced classes as well
  # for instance
  #   rice::and::beans #=> Rice::And::Beans
  def classify
    self.sub(/.*\./, '').split("::").map {|ele| ele.camelcase }.join("::")
  end
  # Constantize tries to find a declared constant with the name specified
  # in the string. It raises a NameError when the name is not in CamelCase
  # or is not initialized.
  #
  # Examples
  #   "Module".constantize #=> Module
  #   "Class".constantize #=> Class
  def constantize(mod=Object)
    camelcased_word = classify
    begin
      mod.module_eval(camelcased_word, __FILE__, __LINE__)
    rescue NameError
      nil
    end
  end
  
  def /(o)
    File.join(self, o.to_s)
  end
  
end