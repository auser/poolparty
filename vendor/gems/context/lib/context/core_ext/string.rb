class String
  # Replaces spaces and tabs with _ so we can use the string as a method name
  # Also replace dangerous punctuation
  def to_method_name
    self.downcase.gsub(/[\s:',\.~;!#=\(\)&]+/,'_')
  end
  
  # Borrowed from +camelize+ in ActiveSupport
  def to_module_name
    self.to_method_name.gsub(/\/(.?)/) { "::#{$1.upcase}" }.gsub(/(?:^|_)(.)/) { $1.upcase }
  end
  
  # Borrowed from +camelize+ in ActiveSupport
  def to_class_name
    self.to_method_name.gsub(/\/(.?)/) { "#{$1.upcase}" }.gsub(/(?:^|_)(.)/) { $1.upcase }
  end
end
