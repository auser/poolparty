class Proc
  def bind(object)
    block, time = self, Time.now
    (class << object; self; end).class_eval do
      method_name = "__bind_#{time.to_i}_#{time.usec}"
      define_method(method_name, &block)
      method = instance_method(method_name)
      remove_method(method_name)
      method
    end.bind(object)
  end
  def in_context(klass_or_obj)
    klass_or_obj.send(:eval, self.to_ruby)
  end
end