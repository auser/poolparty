class Module
  
  # Default accessors
  # Provides an accessor with a default class
  # Usage:
  #   default_attr_reader :a, ["b"]
  def default_attr_reader(sym, default)
    define_method :"#{sym}_default" do
      default
    end
    module_eval "def #{sym};@#{sym} ||= #{sym}_default;end"
  end
  
end