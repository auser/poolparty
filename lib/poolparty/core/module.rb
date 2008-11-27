# Module overloads
class Module
  # Gives us alias_method_chain from rails
  def alias_method_chain(target, feature)
    aliased_target, punctuation = target.to_s.sub(/([?!=])$/, ''), $1
    yield(aliased_target, punctuation) if block_given?

    with_method, without_method = "#{aliased_target}_with_#{feature}#{punctuation}", "#{aliased_target}_without_#{feature}#{punctuation}"

    alias_method without_method, target
    alias_method target, with_method

    case
      when public_method_defined?(without_method)
        public target
      when protected_method_defined?(without_method)
        protected target
      when private_method_defined?(without_method)
        private target
    end
  end
  def attr_accessor_with_default( *syms, &block )
    raise 'Default value in block required' unless block
    syms.each do | sym |
      module_eval do
        attr_writer( sym )
        define_method( sym ) do | |
          class << self; self; end.class_eval do
            attr_reader( sym )
          end
          instance_variables.include?("@#{sym}") ? instance_variable_get( "@#{sym}" ) : instance_variable_set( "@#{sym}", block.call )
        end
      end
    end
    nil
  end
  def instance_variables_from_hash(h={})
    h.each {|k,v| instance_eval "@#{k} = #{v}"}
  end
end